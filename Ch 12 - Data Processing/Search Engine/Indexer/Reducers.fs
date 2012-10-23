// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.Reducers

open System
open System.Collections.Generic

open Indexer.DataTypes
open Indexer.Tokenization

/// Base reducer.
[<AbstractClass>]
type Reducer<'inputKey, 'inputValue, 'output>() =
    abstract member Reduce : 'inputKey * seq<'inputValue> -> seq<'output>

/// Reducer which dedupes token/lexeme pairs. Used to create a lexicon.
type DedupeTokensReducer() =
    inherit Reducer<TokenID, string, Token>()

    default this.Reduce(key, values) =
        // There are going to many 'values' but they should all be identical.
        // Just emitting one Output essentially dedupes them all.
        Seq.singleton { ID = key; Lexeme = Seq.head values }

// Reduces token positions into a basic token posting list.
type TokenPostingListReducer() =
    inherit Reducer<TokenID, TokenPosition, BasicTokenPostingList>()

    default this.Reduce(key, values) =
        // Sort the values by the document ID and then by position.
        let sortedTokenOccurrences = new List<TokenPosition>()
        sortedTokenOccurrences.AddRange(values)
        sortedTokenOccurrences.Sort()
        Seq.singleton { TokenID = key; TokenOccurrences = sortedTokenOccurrences }

// Reducer which just returns all key/value pairs.
// Is it possible to write:
// IdentityReducer<'k, 'v, 'k * 'v>() =
//     inherit Reducer<'k, 'v, 'k * 'v>()
type IdentityReducer() =
    inherit Reducer<int, string, int * string>()

    default this.Reduce(key, values) =
        values |> Seq.map(fun value -> (key, value))