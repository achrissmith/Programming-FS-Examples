// Copyright 2012 Chris Smith. All Rights Reserved.
//
// Local-machine indexer, used for testing.
// NOTE: If you are getting OutOfMemory exceptions, it is probably because you
// are running on a 32-bit OS, or compiling to x86. Since the map/reducing is
// done in memory, a significant data set will easily exceed 1.5G.

open System
open System.IO

open Indexer.DataTypes
open Indexer.Mappers
open Indexer.Reducers

open Indexer.Local.DocumentList
open Indexer.Local.FauxMapReduceFramework

let DefaultCorpusDirectory = @"F:\ProjectGutenbergTextsSmall\"
let DefaultOutputDirectory = @"F:\SearchIndexSmall\"

/// Builds the lexicon for the entire corpus of documents.
let buildLexicon corpusDirectory outputDirectory =
    // Setup the inputs.
    let input = Directory.GetFiles(corpusDirectory, "*.txt")
    let mapper = new ExtractTokenMapper()
    let reducer = new DedupeTokensReducer()

    // Run the map/reduce job locally.
    let lexicon = runMapReduceJob mapper reducer input

    // Serialize the results.
    let filePath = Path.Combine(outputDirectory, "lexicon.txt")
    let fileContent =
        lexicon |> Seq.map (fun token -> token.ToString())
    
    File.WriteAllLines(filePath, fileContent)

    
/// Builds the token posting lists for each token in the corpus.
let buildTokenPostingLists corpusDirectory outputDirectory =
    // Setup the inputs.
    let input = Directory.GetFiles(corpusDirectory, "*.txt")
    let mapper = new DocumentHitListMapper()
    let reducer = new TokenPostingListReducer()

    // Run the map/reduce job locally.
    let tokenPostingLists = runMapReduceJob mapper reducer input

    // Serialize.
    let filePath = Path.Combine(outputDirectory, "postinglists.bin")
    BasicTokenPostingList.SerializeCollection(tokenPostingLists, filePath)
    

/// Builds a list of all known documents.
let buildDocumentList corpusDirectory outputDirectory =
    // Setup the inputs.
    let pathToGutIndex = Path.Combine(corpusDirectory, "GUTINDEX.ALL")

    let input = Directory.GetFiles(corpusDirectory, "*.txt")
    let mapper = new DocumentIndexMapper(pathToGutIndex)
    let reducer = new IdentityReducer()

    // Run the map/reduce job locally.
    let fileNameBookTitlePairs = runMapReduceJob mapper reducer input

    // Serialize.
    let filePath = Path.Combine(outputDirectory, "documentlist.txt")

    let fileContents =
      fileNameBookTitlePairs
      |> Seq.map(fun (docid, bookTitle) -> sprintf "%d| %s" docid bookTitle)
    
    File.WriteAllLines(filePath, fileContents)

[<EntryPoint>]
let main args =
    printfn "Local machine indexer."

    let readLineOrDefault defaultValue = 
        let result = Console.ReadLine()
        if System.String.IsNullOrWhiteSpace(result) then
            defaultValue
        else
            result

    printf "Corpus directory (default: '%s')\n:" DefaultCorpusDirectory
    let corpusDirectory = readLineOrDefault DefaultCorpusDirectory

    printf "Output directory (default: '%s')\n:" DefaultOutputDirectory
    let outputDirectory = readLineOrDefault DefaultOutputDirectory
    if not <| Directory.Exists(outputDirectory) then
        failwithf "Output directory '%s' doesn't exist." outputDirectory

    printfn "Step 1/3: Building lexicon..."
    buildLexicon corpusDirectory outputDirectory

    printfn "Step 2/3: Building token posting lists..."
    buildTokenPostingLists corpusDirectory outputDirectory

    printfn "Step 3/3: Building document list..."
    buildDocumentList corpusDirectory outputDirectory

    printfn "(press any key)"
    Console.ReadKey(true) |> ignore

    0
