// Copyright 2012 Chris Smith. All Rights Reserved.
//
// Simple console application serving as a query interface.

let DocumentList = @"C:\SearchIndex\documentlist.txt"
let TokenPostingLists = @"C:\SearchIndex\postinglists.bin"

open System
open System.IO
open System.Collections.Generic

open Query.Ast
open Query.Parser
open Query.Lexer
open Query.Enumerators

open Microsoft.FSharp.Text.Lexing

open Indexer
open Indexer.DataTypes

/// Loads the entire reverse index in memory. A large enough corpus would need
/// to be sharded across multiple machines.
type Index(documentsFilePath, postingListsFilePath) =
    let loadDocumentLookup() =
        let parseLine (line : string) =
            let parts = line.Split('|')
            if parts.Length <> 2 then
                failwith "Invalid"
            (Int32.Parse(parts.[0]), parts.[1])
        File.ReadAllLines(documentsFilePath)
        |> Seq.map parseLine
        |> Map.ofSeq

    let loadTokenPostingListLookup() =
        BasicTokenPostingList.DeserializeCollection(postingListsFilePath)
        |> Seq.map(fun tpl -> tpl.TokenID, tpl)
        |> Map.ofSeq

    let m_documents = loadDocumentLookup()
    let m_postingLists = loadTokenPostingListLookup()

    member this.DocumentsMap = m_documents
    member this.GetDocument(docID) = 
        if Map.containsKey docID m_documents then m_documents.[docID]
        else sprintf "(document for id %d unknown)" docID

    member this.PostingLists = m_postingLists
    member this.GetPostingList(tokenID) = 
        if Map.containsKey tokenID m_postingLists then m_postingLists.[tokenID]
        else BasicTokenPostingList.Empty(tokenID)

    
/// Map a query object into a single document interator.
let rec getIterator (index : Index) query =
    match query with
    | Term(term) ->
        let tokenID = getTokenID term
        let tokenHitList = index.GetPostingList(tokenID).TokenOccurrences
        printfn "Term with %d hits" <| Seq.length tokenHitList
        let tpl = new TokenPostingList(tokenHitList)
        // It's OK to start at the end. We create no-op token posting lists if the
        // user queries for a term we haven't seen before.
        // if tpl.AtEnd then failwith "Invalid initial state"
        tpl
    | Phrase(phrase) ->
        let individualTokenIterators =
            phrase
            |> List.rev 
            |> List.map getTokenID
            |> List.map(fun tokenID -> index.GetPostingList(tokenID).TokenOccurrences)
            |> List.map(fun tokenHits -> new TokenPostingList(tokenHits))
            |> List.toArray
        createExactPhraseEnumerator individualTokenIterators
    | And(x, y)   -> conjunctiveEnumerator (getIterator index x) (getIterator index y)
    | Or(x, y)    -> disjunctiveEnumerator (getIterator index x) (getIterator index y)
    | SubQuery(q) -> getIterator index q
    | NoOpQuery   -> new TokenPostingList([])

/// Combines a list of query objects into a single one.
let rec joineQueries (queries : Query list) =
    List.fold(fun acc query -> Or(acc, query)) NoOpQuery queries

/// Convert a string input into a Query object
let parseQuery input =
    try
        let lexbuff = LexBuffer<char>.FromString(input)

        printfn "Lexed:"
        while not lexbuff.IsPastEndOfStream do
            let token = Query.Lexer.tokenize lexbuff
            printfn "\tToken %A" token

        // Reset the lex buf since we chewed through it to print debug tokens.
        let lexbuff2 = LexBuffer<char>.FromString(input)
        let query = 
            Query.Parser.start Query.Lexer.tokenize lexbuff2
            
        printfn "Parsed: %A" query
        query
    with ex ->
        printfn "Parse error: %s" ex.Message
        [ NoOpQuery ]

[<EntryPoint>]
let main (args : string array) = 
    printfn "Query System"

    printfn "Loading index..."
    let index = new Index(DocumentList, TokenPostingLists)

    printfn "%d documents with %d tokens in index."
        index.DocumentsMap.Count index.PostingLists.Count

    let mutable input = ""
    printf ":"
    input <- Console.ReadLine()

    while input <> "quit" do
        let query = parseQuery input
        let joinedQuery = joineQueries query

        let resultIterator = getIterator index joinedQuery

        printfn "Results =\n"
        if resultIterator.AtEnd then
            printfn "No hits."
        else
            // Note that some iterators only return the first hit in a document. (To avoid
            // wasting time spinning in iterators.)
            seq {
                while not resultIterator.AtEnd do
                    let result = resultIterator.Current
                    let document = index.GetDocument(result.DocumentID)
                    let positition = result.DocumentPosition
                    yield (document, positition)
                    resultIterator.MoveNextDocument()
            }
            |> Seq.iter(fun (doc, position) -> printfn "%s @ %d" doc position)
        
        // Print blank line
        printfn ""

        // Read next query
        printf ":"
        input <- Console.ReadLine()
    done

    Console.WriteLine("(press any key)")
    Console.ReadKey(true) |> ignore
    
    0
