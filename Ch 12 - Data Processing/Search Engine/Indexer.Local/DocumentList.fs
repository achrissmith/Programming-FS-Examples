// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.Local.DocumentList

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

open Indexer.DataTypes
open Indexer.Mappers
open Indexer.Reducers

// Use a regular expression to capture three groups
let (|RegexMatch2|_|) (pattern : string) (input : string) =
    let result = Regex.Match(input, pattern)
    if result.Success then
        match (List.tail [ for g in result.Groups -> g.Value ]) with
        | fst :: snd :: []
              -> Some (fst, snd)
        | [] -> failwith <| "Match succeeded, but no groups found.\n" +
                            "Use '(.*)' to capture groups"
        | _  -> failwith "Match succeeded, but did not find exactly three groups."
    else
        None

let tryParseLine input =
    match input with
    | RegexMatch2 "(.+) (\d+)" (title, etextNumber) -> Some(title, etextNumber)
    | _ -> None

let tryParseEtextNumber input =
    match input with
    | RegexMatch2 "\[(\w+) #(\d+)\]" ("Etext", etextNumber) -> Some(etextNumber)
    | _ -> None


// Given a filepath to a "GUTINDEX.ALL" file, returns a Dictionary from etext ("768") to
// book title ("Wuthering Heights, by Emily Bronte").
let generateEtextTitleMapping filePath =
    let catalogFiles = new Dictionary<string, string>()

    let addTitle title textNumber =
        if catalogFiles.ContainsKey(textNumber) then
            catalogFiles.[textNumber] <- title
        else
            catalogFiles.Add(textNumber, title)

    File.ReadAllLines(filePath)
    |> Seq.map tryParseLine
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.iter(fun (title, textNumber) -> addTitle title textNumber)

    printfn "Catalog files: %d" catalogFiles.Count

    catalogFiles

/// Maps a file path to a docid x book title tuple.
type DocumentIndexMapper(indexFile) =
    inherit Mapper<DocumentID, string>()

    // Generate a dictionary from etext ID to book title.
    let etextToBookTitleLookup = generateEtextTitleMapping indexFile

    default this.Map(filePath) =
        let fileName = Path.GetFileName(filePath)
        let docID = fileName.GetHashCode()

        File.ReadAllLines(filePath)
        |> Seq.map tryParseEtextNumber
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.map(fun etextNum -> (docID, etextToBookTitleLookup.[etextNum]))
