// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.DataTypes

open System
open System.Collections.Generic
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open System.Text

/// Raw, unprocessed token. An individual word within a document. For example:
/// "Hello"; ","; "World"; "!".
type RawToken = string

/// Unique ID to represent a token. Obtained by taking the has hashcode of the
/// raw token.
type TokenID = int

/// Converts a RawToken into a TokenID
let getTokenID (rawToken : RawToken) : TokenID =
    rawToken.ToLowerInvariant().GetHashCode()

/// Fancy way to represent a token id and lexeme tuple.
type Token =
    { ID: TokenID; Lexeme: string }
    override this.ToString() =
        sprintf "%d: %s" this.ID this.Lexeme

type Lexicon = Map<TokenID, Token>

/// Unique ID to represent a document. Obtained by taking the hashcode of the
// document name.
type DocumentID = int

/// 0-indexed position within a document.
type DocumentPosition = int

/// Fancy way to represent a document ID and position tuple. Note that this
/// type implements custom comparison and equality.
///
/// TokenPositions are sorted by document id first, and then document position.
[<CustomComparison; CustomEquality>]
type TokenPosition =
    { DocumentID : DocumentID; DocumentPosition : DocumentPosition }

    /// Placeholder position for invalid locations. (E.g. an ended iterator.)
    static member InvalidTokenPosition =
        { DocumentID = 0xffff; DocumentPosition = 0xffff }

    interface IComparable with
        member this.CompareTo((rhs : obj)) =
            let rhsAsTokenPosition = rhs :?> TokenPosition
            let thisAsIComparable = this :> IComparable<TokenPosition>
            thisAsIComparable.CompareTo(rhsAsTokenPosition)

    interface IComparable<TokenPosition> with
        member this.CompareTo((rhs : TokenPosition)) =
            let lhs = this
            if lhs.DocumentID < rhs.DocumentID then
                -1
            elif lhs.DocumentID = rhs.DocumentID then
                compare lhs.DocumentPosition rhs.DocumentPosition
            else
                1

    // Parses a token position from text.
    static member Parse(str : string) =
        let throwInvalidException() =
            raise <| new ArgumentException("Invalid format.")
        if (str.[0] <> '{') || (str.[str.Length - 1] <> '}') then
            throwInvalidException()

        let parts = str.Substring(1, str.Length - 2).Split(',')
        if parts.Length <> 2 then
            throwInvalidException()

        try
            { DocumentID = Int32.Parse(parts.[0])
              DocumentPosition = Int32.Parse(parts.[1]) }
        with _ -> 
            throwInvalidException()

    override this.ToString() =
        sprintf "{%d,%d}" this.DocumentID this.DocumentPosition

    override this.GetHashCode() = (this.DocumentID ^^^ this.DocumentPosition)

    override this.Equals(o) =
        let thisAsIC = this :> IComparable<TokenPosition>
        match o with
        | null -> false
        | :? TokenPosition as o -> (thisAsIC.CompareTo(o) = 0)
        | _ -> false

/// Posting list for a given token. Note that the token occurences are
/// sorted.
type BasicTokenPostingList = 
    { TokenID : TokenID; TokenOccurrences : seq<TokenPosition> }

    /// Creates a new, empty BasicTokenPostingList.
    static member Empty(tokenID) = { TokenID = tokenID; TokenOccurrences = [] }
        
    // Format: Num of posting lists, (TokenID, token position count, token positions)*
    member this.SerializeTo(stream : BinaryWriter) =
        let numHits = Seq.length this.TokenOccurrences
        // printfn "Serializing TPL for token [%d] with length %d" this.TokenID numHits

        stream.Write(int32 this.TokenID)
        stream.Write(int32 numHits)

        this.TokenOccurrences
        |> Seq.iter(fun tokenOccur -> stream.Write(int32 tokenOccur.DocumentID)
                                      stream.Write(int32 tokenOccur.DocumentPosition))

    static member ParseFrom(stream : BinaryReader) =
        let tokenID = stream.ReadInt32()
        let tokenOccurrenceCount = stream.ReadInt32()
        // printfn "Deserializing TPL for token [%d] with length %d" tokenID tokenOccurrenceCount

        let tokenOccurrences = Array.zeroCreate tokenOccurrenceCount
        for i = 0 to tokenOccurrenceCount - 1 do
            let docID = stream.ReadInt32()
            let docPos = stream.ReadInt32()
            let tokenOccurrence = { DocumentID = docID; DocumentPosition = docPos }
            tokenOccurrences.[i] <- tokenOccurrence

        { TokenID = tokenID; TokenOccurrences = tokenOccurrences :> seq<_> }

    static member SerializeCollection(collection : seq<BasicTokenPostingList>, filePath) =
        use fileWriter = new StreamWriter(filePath, false (* append *))
        use binWriter = new BinaryWriter(fileWriter.BaseStream)
    
        binWriter.Write(int32 <| Seq.length collection)

        collection
        |> Seq.iter (fun tpl -> tpl.SerializeTo(binWriter))
    
        fileWriter.Close()

    static member DeserializeCollection(filePath : string) =
        use fileReader = new StreamReader(filePath)
        use binReader = new BinaryReader(fileReader.BaseStream)

        let numTpls = binReader.ReadInt32()

        let tpls = new List<_>(numTpls)
        for i = 0 to numTpls - 1 do
            let tpl = BasicTokenPostingList.ParseFrom(binReader)
            tpls.Add(tpl)

        fileReader.Close()

        tpls
