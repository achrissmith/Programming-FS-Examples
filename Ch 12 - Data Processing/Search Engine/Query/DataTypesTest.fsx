#r "FSharp.PowerPack.dll"

// Contains the definition of the final data type:
// type Query =
//     | Term of string
//     | Phrase of string list
//     | And of Query * Query
//     | Or of Query * Query
//     | SubQuery of Query
//     | NoOpQuery
#load "QueryTypes.fs"
// Parser generated from fsyacc
#load "QueryParser.fs"
// Lexer generated from fslex
#load "QueryLexer.fs"

open System
open Microsoft.FSharp.Text.Lexing

// Parse a query.
let parseQuery rawQuery =
    printfn "Lexing [%s]" rawQuery
    
    let lexbuff = LexBuffer<char>.FromString(rawQuery)
    while not lexbuff.IsPastEndOfStream do
        let token = Query.Lexer.tokenize lexbuff
        printfn "Token %A" token

    printfn "Parsing"
    let lexbuff2 = LexBuffer<char>.FromString(rawQuery)
    let query = 
        Query.Parser.start Query.Lexer.tokenize lexbuff2
            
    printfn "Query =\n%A" query

// Simple two-term query.
parseQuery "Hello, World"

// Use of phrase and conjunction.
parseQuery "\"Super excited\" OR \"mega awesome\""
