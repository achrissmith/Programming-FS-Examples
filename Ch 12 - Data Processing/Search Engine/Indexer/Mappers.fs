// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.Mappers

open System
open System.IO
open System.Collections.Generic

open Indexer.DataTypes
open Indexer.Tokenization

/// Base mapper.
/// 'k and 'v make up the key/value pairs on output. Takes a string input.
[<AbstractClass>]
type Mapper<'k, 'v when 'k : equality>() =
    abstract member Map : string -> seq<'k * 'v>

/// Mapper which maps an input file to a series of token and lexemes.
type ExtractTokenMapper() =
    inherit Mapper<TokenID, string>()

    default this.Map(fileName) =
        let docID = fileName.GetHashCode()

        breakFile fileName
        |> Seq.map(fun rawToken -> rawToken.GetHashCode(), rawToken)
        |> Seq.map(fun (tokenID, lexeme) -> (tokenID, lexeme))

// Mapper and Reducer to create Token-major posting lists.
type DocumentHitListMapper() =
    inherit Mapper<TokenID, TokenPosition>()

    default this.Map(filePath) =
        let fileName = Path.GetFileName(filePath)
        let docID = fileName.GetHashCode()

        tokenizeFile filePath
        |> Seq.mapi(fun i token -> token, { DocumentID = docID; DocumentPosition = i })
        |> Seq.map(fun (token, tokenPosition) -> (token, tokenPosition))
