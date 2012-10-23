#load "DataTypes.fs"
#load "Tokenization.fs"

open Indexer.Core.Tokenization

// Snippet from "dying is fine)but Death"
// By e.e. cummings
let poem = @"
dying is fine)but Death

?o
baby
i

wouldn't like

Death if Death
were
good:for

when(instead of stopping to think)you

begin to feel of it,dying
's miraculous
why?be

cause dying is
..."

let tokens = tokenizeText poem