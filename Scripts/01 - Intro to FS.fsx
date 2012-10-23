module ProgrammingFS.Ch1

// ----------------------------------------------------------------------------

// HelloWorld.fs

printfn "Hello, World"

// ----------------------------------------------------------------------------

/// Compute the greatest common divisor of
/// two numbers. 
let rec gcd x y =
    if y = 0 then x
    else gcd y (x % y)

let x = gcd 1024 12

// ----------------------------------------------------------------------------

(* Defines a program which takes two command line
arguments and prints them, along with the current
time, to the console. *)

open System

[<EntryPoint>]
let main (args : string[]) =

    if args.Length <> 2 then
        failwith "Error: Expected arguments <greeting> and <thing>"
        
    let greeting, thing = args.[0], args.[1]
    let timeOfDay = DateTime.Now.ToString("hh:mm tt")
    
    printfn "%s, %s at %s" greeting thing timeOfDay
    
    // Program exit code
    0