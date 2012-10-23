// Copyright 2012 Chris Smith. All Rights Reserved.
// TODO(chrsmith): Actually test this.

#load "DataTypes.fs"
#load "Tokenization.fs"
#load "FauxMapReduceFramework.fs"
#load "Enumerators.fs"

open System
open System.IO

open Indexer.Core.DataTypes
open Indexer.Core.Tokenization
open Indexer.Core.FauxMapReduceFramework
open Indexer.Core.Enumerators

let corpus = [
    "alpha.text", "This is the first document.";
    "beta.text",  "This is the second document.";
    "gamma.text", "This is the third document." ]

// Build up token posting lists.
/// Posting lists for every type of token in the corpus.
let tokenPostingLists =
    corpus
    |> Seq.map(fun (docName, docText) -> let tempFilePath = Path.GetTempPath() + docName
                                         File.WriteAllText(tempFilePath, docText)
                                         tempFilePath)
    |> Seq.map(fun fileToProcess -> let mapper = new DocumentHitListMapper()
                                    mapper.Map(fileToProcess)
                                    mapper)
    |> Seq.map(fun mapper -> mapper.Results)
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map(fun (tokenId, tokenPositions) -> let tokenPositions' =
                                                    tokenPositions
                                                    |> Seq.map snd
                                                tokenId, tokenPositions')
    |> Seq.map(fun (tokenId, tokenPositions) -> let reducer = new TokenPostingListReducer()
                                                reducer.Reduce(tokenId, tokenPositions)
                                                reducer.Results)
    |> Seq.concat

/// Document lookup.
let docNameLookup =
    corpus
    |> Seq.map(fun (docName, _) -> Path.GetTempPath() + docName)
    |> Seq.map(fun filePath -> filePath.GetHashCode(), filePath)
    |> Map.ofSeq

/// Posting list dictionary.
let plDictionary =
    let foldFunction acc (tokenPlList : BasicTokenPostingList) =
        Map.add tokenPlList.TokenID tokenPlList.TokenOccurrences acc
    tokenPostingLists
    |> Seq.fold foldFunction Map.empty

/// Returns the token iterator for a given token.
let tokenEnumeratorFromId tokenId =
    if plDictionary.ContainsKey tokenId then
        new TokenPostingList(plDictionary.[tokenId])
    else
        new TokenPostingList([ (* empty posting list *) ])

let tokenEnumeratorFromText = getTokenID >> tokenEnumeratorFromId

let createExactPhraseEnumeratorFromPhrase phrase =
    breakText phrase
    |> tokenizeText
    |> Array.map tokenEnumeratorFromId
    |> createExactPhraseEnumerator

let prettyPrintHits (iterator : TokenPostingList) =
    while not <| iterator.AtEnd do
        let tokenHit = iterator.Current

        let documentName = docNameLookup.[tokenHit.DocumentID]
        printfn "%s at %d" documentName tokenHit.DocumentPosition

        iterator.MoveNext()

// prettyPrintHits <| tokenEnumeratorFromText "this";;
// prettyPrintHits <| createExactPhraseEnumeratorFromPhrase "This is the"
// prettyPrintHits <| createExactPhraseEnumeratorFromPhrase "This is the first"