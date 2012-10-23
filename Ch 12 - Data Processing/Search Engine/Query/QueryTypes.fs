// Copyright 2012 Chris Smith. All Rights Reserved.

module Query.Ast

type Query =
    | Term of string
    | Phrase of string list
    | And of Query * Query
    | Or of Query * Query
    | SubQuery of Query
    | NoOpQuery