module ProgrammingFS.Ch04

if Unchecked.defaultof<string> <> null then failwith "ERROR"
if Unchecked.defaultof<decimal> <> 0M then failwith "ERROR"

let isNull = function null -> true | _ -> false

// ----------------------------------------------------------------------------

#if COMPILEERROR
type Thing = Plant | Animal | Mineral;;

// ERROR: Thing cannot be null
let testThing thing =
    match thing with
    | Plant   -> "Plant"
    | Animal  -> "Animal"
    | Mineral -> "Mineral"
    | null    -> "(null)"

#endif

// ----------------------------------------------------------------------------

let x = [| 0 |]
let y = x;;
x.[0] <- 3;;
x;;
y;;

// ERROR: Cannot use mutable values except in the function they are defined
let invalidUseOfMutable() =
    let mutable x = 0
    let incrementX() = x <- x + 1
    incrementX()
    // Return x
    x;;

// ----------------------------------------------------------------------------

let planets =
    ref [
        "Mercury";  "Venus";     "Earth";
        "Mars";     "Jupiter";   "Saturn";
        "Uranus";   "Neptune";   "Pluto"
    ]

// Oops! Sorry Pluto...

// Filter all planets not equal to "Pluto"
// Get the value of the planets ref cell using (!),
// then assign the new value using (:=)
planets := !planets |> List.filter (fun p -> p <> "Pluto") 

// ----------------------------------------------------------------------------

// The incr and decr functions
let x = ref 0
incr x
printfn "Zero after incr = %d" !x
decr x
decr x
printfn "One after decr, decr = %d" !x;;

// ----------------------------------------------------------------------------

// Mutable record types
open System

type MutableCar = { Make : string; Model : string; mutable Miles : int }

let driveForASeason car =
    let rng = new Random()
    car.Miles <- car.Miles + rng.Next() % 10000;;

let kitt = { Make = "Pontiac"; Model = "Trans Am"; Miles = 0 }

driveForASeason kitt
driveForASeason kitt
driveForASeason kitt
driveForASeason kitt;;

// ----------------------------------------------------------------------------

// Using the array comprehension syntax
let perfectSquares = [| for i in 1 .. 7 -> i * i |]

// Manually declared
let perfectSquares2 = [| 1; 4; 9; 16; 25; 36; 49; 64; 81 |];;

// Indexing an arra
printfn
    "The first three perfect squares are %d, %d, and %d"
    perfectSquares.[0]
    perfectSquares.[1]
    perfectSquares.[2];;

// ----------------------------------------------------------------------------

open System

// Encrypt a letter using ROT13
let rot13Encrypt (letter : char) =
    
    // Move the letter forward 13 places in the alphabet (looping around) 
    // Otherwise ignore.    
    if Char.IsLetter(letter) then
        let newLetter = 
            (int letter)
            |> (fun letterIdx -> letterIdx - (int 'A'))
            |> (fun letterIdx -> (letterIdx + 13) % 26)
            |> (fun letterIdx -> letterIdx + (int 'A'))
            |> char
        newLetter
    else
        letter
    
// Loop through each array element, encrypting each letter    
let encryptText (text  : char[]) =
    
    for idx = 0 to text.Length - 1 do
        let letter = text.[idx]
        text.[idx] <- rot13Encrypt letter
        
let text = 
    Array.ofSeq "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG"

printfn "Origional = %s" <| new String(text)
encryptText(text)
printfn "Encrypted = %s" <| new String(text)

// A unique trait of ROT13 is that to decrypt, simply encrypt again
encryptText(text)
printfn "Decrypted = %s" <| new String(text)

// ----------------------------------------------------------------------------

open System
let daysOfWeek = Enum.GetNames( typeof<DayOfWeek> )

// Standard array slice, elements 2 through 4
daysOfWeek.[2..4]

// Just specify lower bound, elements 4 to the end
daysOfWeek.[4..]

// Just specify an upper bound, elements 0 to 2
daysOfWeek.[..2]

// Specify no bounds, get all elements (copies the array)
daysOfWeek.[*]

// ----------------------------------------------------------------------------

// Initialize an array of sin-wave elements
let divisions = 4.0
let twoPi = 2.0 * Math.PI;;

Array.init (int divisions) (fun i -> float i * twoPi / divisions);;

// Construct empty arrays
let emptyIntArray    : int array    = Array.zeroCreate 3
let emptyStringArray : string array = Array.zeroCreate 3;;

// ----------------------------------------------------------------------------

// Describe an array
let describeArray arr =
    match arr with
    | null       -> "The array is null"
    | [| |]      -> "The array is empty"
    | [| x |]    -> sprintf "The array has one element, %A" x
    | [| x; y |] -> sprintf "The array has two elements, %A and %A" x y
    | a -> sprintf "The array had %d elements, %A" a.Length a;;

describeArray [| 1 .. 4 |];;

describeArray [| ("tuple", 1, 2, 3) |];;

// Simple Boolean function
let rec isPowerOfTwo x =
    if x = 2 then
        true
    elif x % 2 = 1  (* is odd *) then
        false
    else isPowerOfTwo (x / 2);;

[| 1; 7; 13; 64; 32 |]
|> Array.tryFind isPowerOfTwo;;

[| 1; 7; 13; 64; 32 |]
|> Array.tryFindIndex isPowerOfTwo;;

// ----------------------------------------------------------------------------

let vowels = [| 'a'; 'e'; 'i'; 'o'; 'u' |];;

Array.iteri (fun idx chr -> printfn "vowel.[%d] = %c" idx chr) vowels;;

// ----------------------------------------------------------------------------

// Creating a 3x3 array
let identityMatrix : float[,] = Array2D.zeroCreate 3 3
identityMatrix.[0,0] <- 1.0
identityMatrix.[1,1] <- 1.0
identityMatrix.[2,2] <- 1.0;;

identityMatrix;;

// Create a grid 1-9 
Array2D.init 3 3 (fun r c -> c + r*3 + 1);;

Array3D.init 2 2 2 (fun x y z -> sprintf "%d,%d,%d" x y z);;


// Create a jagged array
let jaggedArray : int[][]  = Array.zeroCreate 3
jaggedArray.[0] <- Array.init 1 (fun x -> x)
jaggedArray.[1] <- Array.init 2 (fun x -> x)
jaggedArray.[2] <- Array.init 3 (fun x -> x);;

jaggedArray;;

// ----------------------------------------------------------------------------

// Create a List<_> of planets
open System.Collections.Generic
let planets = new List<string>();;


// Add individual planets
planets.Add("Mercury")
planets.Add("Venus")
planets.Add("Earth")
planets.Add("Mars");;

planets.Count;;

// Add a collection of values at once
planets.AddRange( [| "Jupiter"; "Saturn"; "Uranus"; "Neptune"; "Pluto" |] );;

planets.Count;;

// Sorry bro
planets.Remove("Pluto");;

planets.Count;;

// ----------------------------------------------------------------------------

// Atomic Mass Units 
[<Measure>]
type amu 

type Atom = { Name : string; Weight : float<amu> }

open System.Collections.Generic
let periodicTable = new Dictionary<string, Atom>()

periodicTable.Add( "H", { Name = "Hydrogen";  Weight = 1.0079<amu> })
periodicTable.Add("He", { Name = "Helium";    Weight = 4.0026<amu> })
periodicTable.Add("Li", { Name = "Lithium";   Weight = 6.9410<amu> })
periodicTable.Add("Be", { Name = "Beryllium"; Weight = 9.0122<amu> })
periodicTable.Add( "B", { Name = "Boron ";    Weight = 10.811<amu> })
// ...

// Lookup an element 
let printElement name =
    
    if periodicTable.ContainsKey(name) then
        let atom = periodicTable.[name]
        printfn 
            "Atom with symbol with '%s' has weight %A."
            atom.Name atom.Weight
    else
        printfn "Error. No atom with name '%s' found." name

// Alternate syntax to get a value. Return a tuple of 'success * result'
let printElement2 name =
    
    let (found, atom) = periodicTable.TryGetValue(name)
    if found then
        printfn 
            "Atom with symbol with '%s' has weight %A."
            atom.Name atom.Weight
    else
        printfn "Error. No atom with name '%s' found." name

// ----------------------------------------------------------------------------
        
// While loop
let mutable i = 0
while i < 5 do
    i <- i + 1
    printfn "i = %d" i;;

// For loop
for i = 0 to 5 do
    printfn "i = %d" i;;
    
// Counting down
for i = 5 downto 1 do
    printfn "%d" i;;

// Enumerable loop
for i in [1 .. 5] do
    printfn "%d" i;;

// ----------------------------------------------------------------------------

// Pet type
type Pet =
    | Cat of string * int // Name, Lives
    | Dog of string       // Name

let famousPets = [ Dog("Lassie"); Cat("Felix", 9); Dog("Rin Tin Tin") ];;

// Print famous dogs
for Dog(name) in famousPets do
    printfn "%s was a famous dog." name

// ----------------------------------------------------------------------------
    
// Using failwithf
let divide x y =
    if y = 0 then failwithf "Cannot divide %d by zero!" x
    x / y

// Raising a DivideByZeroException
let divide2 x y =
    if y = 0 then raise <| new System.DivideByZeroException()
    x / y

// ----------------------------------------------------------------------------

open System.IO

[<EntryPoint>]
let main (args : string[]) =

    let exitCode =
        try
            let filePath = args.[0]
            
            printfn "Trying to gather information about file:"
            printfn "%s" filePath
            
            // Does the drive exist?
            let matchingDrive =
                Directory.GetLogicalDrives()
                |> Array.tryFind (fun drivePath -> drivePath.[0] = filePath.[0])
            
            if matchingDrive = None then 
                raise <| new DriveNotFoundException(filePath)
            
            // Does the folder exist?
            let directory = Path.GetPathRoot(filePath)
            if not <| Directory.Exists(directory) then
                raise <| new DirectoryNotFoundException(filePath)
                
            // Does the file exist?
            if not <| File.Exists(filePath) then
                raise <| new FileNotFoundException(filePath)
            
            let fileInfo = new FileInfo(filePath)
            printfn "Created  = %s" <| fileInfo.CreationTime.ToString()
            printfn "Access   = %s" <| fileInfo.LastAccessTime.ToString()
            printfn "Size     = %d" fileInfo.Length
            
            0
            
        with
        // Combine patterns using Or
        | :? DriveNotFoundException
        | :? DirectoryNotFoundException
            ->  printfn "Unhandled Drive or Directory not found exception"
                1
        | :? FileNotFoundException as ex
            ->  printfn "Unhandled FileNotFoundException: %s" ex.Message
                3
        | :? IOException as ex
            ->  printfn "Unhandled IOException: %s" ex.Message
                4
        // Use a wildcard match (result will be of type System.Exception)
        | _ as ex 
            ->  printfn "Unhandled Exception: %s" ex.Message
                5

    // Return the exit code
    printfn "Exiting with code %d" exitCode
    exitCode

// ----------------------------------------------------------------------------

// Try-finally expressions
let tryFinallyTest() =
    try
        printfn "Before exception..."
        failwith "ERROR!"
        printfn "After exception raised..."
    finally
        printfn "Finally block executing..."

let test() =
    try
        tryFinallyTest()
    with
    | ex -> printfn "Exception caught with message: %s" ex.Message;;

// ----------------------------------------------------------------------------

open Checked

let rethrowExceptionTest() =
    try
        let x = 0x0fffffff
        let y = 0x0fffffff
        
        x * y
    with
    | :? System.OverflowException as ex
        ->  printfn "An overflow exception occured..."
            rethrow()

// ----------------------------------------------------------------------------

#r "System.Core.dll"
open System
open System.Collections.Generic

exception NoMagicWand
exception NoFullMoon of int * int
exception BadMojo of string

let castHex (ingredients : HashSet<string>) =
    try
        
        let currentWand = Environment.MagicWand
        
        if currentWand = null then
            raise NoMagicWand
    
        if not <| ingredients.Contains("Toad Wart") then
            raise <| BadMojo("Need Toad Wart to cast the hex!")
        
        if not <| isFullMoon(DateTime.Today) then
            raise <| NoFullMoon(DateTime.Today.Month, DateTime.Today.Day)
         
        // Begin the incantation...
        let mana =
            ingredients
            |> Seq.map (fun i -> i.GetHashCode())
            |> Seq.fold (+) 0  

        sprintf "%x" mana

    with
    | NoMagicWand 
        -> "Error: A magic wand is required to hex!"
    | NoFullMoon(month, day)
        -> "Error: Hexes can only be cast during a full moon."
    | BadMojo(msg)
        -> sprintf "Error: Hex failed due to bad mojo [%s]" msg 
