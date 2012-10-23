module ProgrammingFS.Ch2

// ----------------------------------------------------------------------------

let answerToEverything = 42UL

let pi = 3.1415926M

let avogadro = 6.022e-23

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let hex = 0xFCAF

let oct = 0o7771L

let bin = 0b00101010y

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let _ = 0x401E000000000000LF

let _ = 0x00000000lf

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if 32767s + 1s <> -32768s then failwith "ERROR"

if -32768s - 1s <> 32767s then failwith "ERROR"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Data storage units
let megabyte  = 1024I    * 1024I
let gigabyte  = megabyte * 1024I
let terabyte  = gigabyte * 1024I
let petabyte  = terabyte * 1024I
let exabyte   = petabyte * 1024I
let zettabyte = exabyte  * 1024I

// ----------------------------------------------------------------------------

// Print the truth table for the given function
let printTruthTable f =
    printfn "       |true   | false |"
    printfn "       +-------+-------+"
    printfn " true  | %5b | %5b |" (f true true)  (f true false)
    printfn " true  | %5b | %5b |" (f false true) (f false false)
    printfn "       +-------+-------+"
    printfn ""
    ()
    
printTruthTable (&&)

printTruthTable (||)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let square x = x * x

let addOne x = x + 1

let addTwo x y = x + y

(*
let add x y = x + y;;

add 1.0 2.0;;

let add x y = x + y
add 1.0 2.0;;

*)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Convert byte to gigabytes
let bytesToGB x =
    let x = x / 1024I   //  B to KB
    let x = x / 1024I   // KB to MB
    let x = x / 1024I   // MB to GB
    x
    
let hardDriveSize = 250I * 1024I * 1024I * 1024I

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let dinner = ("green egg", "ham")

let zeros = (0, 0L, 0.0)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Simple lit comprehenion
let numbersNear x =
    [
        yield x - 1
        yield x
        yield x + 1
    ]
    
// More complex list comprehensions
let x =
    [   let negate x = -x
        for i in 1 .. 10 do
            if i % 2 = 0 then
                yield negate i
            else
                yield i ]
                
// Generate the first ten multiples of a number
let multiplesOf x = [ for i in 1 .. 10 do yield x * i ]

// Simplified list comprehension
let multiplesOf2 x = [ for i in 1 .. 10 -> x * i ]
                
// List comprehension for prime numbers
let primesUnder max =
    [
        for n in 1 .. max do
            let factorsOfN =
                [ 
                    for i in 1 .. n do
                        if n % i = 0 then
                            yield i 
                ]
            
            // n is prime if its only factors are 1 and n
            if List.length factorsOfN = 2 then
                yield n
    ]
        
// ----------------------------------------------------------------------------

let isMultipleOf5 x = (x % 5 = 0)

let multOf5, nonMultOf5 =
    List.partition isMultipleOf5 [1 .. 15]

// Count the number of vowels in a string
let countVowels (str : string) =
    let charList = List.ofSeq str

    let accFunc (As, Es, Is, Os, Us) letter =
        if   letter = 'a' then (As + 1, Es, Is, Os, Us)
        elif letter = 'e' then (As, Es + 1, Is, Os, Us)
        elif letter = 'i' then (As, Es, Is + 1, Os, Us)
        elif letter = 'o' then (As, Es, Is, Os + 1, Us)
        elif letter = 'u' then (As, Es, Is, Os, Us + 1)
        else                   (As, Es, Is, Os, Us)

    List.fold accFunc (0, 0, 0, 0, 0) charList

// Using Option.get
let isLessThanZero x = (x < 0)

let containsNegativeNumbers intList =
    let filteredList = List.filter isLessThanZero  intList
    if List.length filteredList > 0
    then Some(filteredList)
    else None

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let inferParams x y z =
    printfn "x = %f, y = %s, z = %b" x y z
    
// ----------------------------------------------------------------------------

module ProgramFS = 

    // Program.fs
    let numbers = [1 .. 10]
    let square x = x * x

    let squaredNumbers = List.map square numbers

    printfn "SquaredNumbers = %A" squaredNumbers

    open System

    printfn "(press any key to continue)"
    Console.ReadKey(true)
