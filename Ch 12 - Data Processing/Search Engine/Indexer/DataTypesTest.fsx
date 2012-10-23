#load "DataTypes.fs"

open System

open Indexer.DataTypes

// Comparison tests
let alpha1 = { DocumentID = 0; DocumentPosition = 0 }
let alpha2 = { DocumentID = 0; DocumentPosition = 1 }
let alpha3 = { DocumentID = 0; DocumentPosition = 2 }
let beta1  = { DocumentID = 1; DocumentPosition = 0 }
let beta2  = { DocumentID = 1; DocumentPosition = 2 }
let beta3  = { DocumentID = 1; DocumentPosition = 3 }

let testCmp (x : TokenPosition) (y : TokenPosition) expected =
    let result = (x :> IComparable<TokenPosition>).CompareTo(y)
    if result <> expected then
        failwith "Test failure."

testCmp alpha2 alpha1  1
testCmp alpha2 alpha2  0
testCmp alpha2 alpha3 -1

testCmp alpha1 beta1  -1
testCmp beta1  alpha1  1

let testEq (x : TokenPosition) (y : obj) expected =
    if x.Equals(y) <> expected then
        failwith "Equality failure."

testEq alpha1 "foo"  false
testEq alpha1 alpha2 false
testEq alpha1 alpha1 true

let testStrEq (s1 : string) (s2 : string) expected =
    if (s1 = s2) <> expected then   
        failwith "String comparison failure."

testStrEq "{0,0}" (alpha1.ToString()) true
testStrEq "{1,0}" (alpha1.ToString()) false

let testIntEq (x : int) (y : int) expected =
    if (x = y) <> expected then
        failwith "Int comparison failure."


let alpha1Copy = TokenPosition.Parse("{13,37}")
testIntEq alpha1Copy.DocumentID 13 true
testIntEq alpha1Copy.DocumentPosition 37 true

