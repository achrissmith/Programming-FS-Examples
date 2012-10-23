// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.Tokenization

open System
open System.IO

open Indexer.DataTypes

/// We intentionally omit the apostrophe since "it's" should be considered
/// a single word. Same for the hypen.
let delimiters = [| ' '; '\t'; '\r'; '\n';
                    ';'; '('; ')'; '\"';
                    ','; '.';  '!'; '?'; '<'; '>'; '/'; '\\'; '&'; '='; '~';
                    '+'; '-' |]

/// Breaks text into raw tokens.
let breakText (text : string) : RawToken[] =
    text.Split(delimiters, StringSplitOptions.RemoveEmptyEntries)

/// Breaks a file into an array of raw tokens.
let breakFile filePath : RawToken[] = 
    File.ReadAllText(filePath)
    |> breakText

/// Tokenizes an array of raw tokens.
let tokenizeText (rawTokens : RawToken[]) =
    rawTokens
    |> Array.map(fun rawToken -> getTokenID rawToken)

/// Tokenizes a file.
let tokenizeFile filePath = 
    breakFile filePath
    |> tokenizeText


