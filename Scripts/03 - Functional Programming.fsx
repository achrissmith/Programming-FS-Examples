module ProgrammingFS.Ch03

let square x = x * x

let imperativeSum numbers =
    let mutable total = 0
    for i in numbers do
        let x = square i
        total <- total + x
    total

let functionalSum numbers =
    numbers
    |> Seq.map square
    |> Seq.sum

// ----------------------------------------------------------------------------

let negate x = -x

List.map negate [1 .. 10]

List.map (fun i -> -i) [1 .. 10]

// ----------------------------------------------------------------------------

// Append text to a file
open System.IO

let appendFile (fileName : string) (text : string) =
    use file = new StreamWriter(fileName, true)
    file.WriteLine(text)
    file.Close()

//appendFile @"D:\Log.txt" "Processing Event X..."

// ----------------------------------------------------------------------------

// Curry appendFile so that the first parameter is fixed
let curriedAppendFile = appendFile @"D:\Log.txt";;

// Appends text to to 'D:\Log.txt'
curriedAppendFile "Processing Event Y...";;

// ----------------------------------------------------------------------------

// Functions returning functions
let generatePowerOfFunc x =
    (fun y -> x ** y)

let powerOfTwo = generatePowerOfFunc 2.0

let powerOfThree = generatePowerOfFunc 3.0

// ----------------------------------------------------------------------------

let rec factorial x =
    if x <= 1 then
        1
    else 
        x * factorial (x - 1)
        
// ----------------------------------------------------------------------------

open System

// Functional for loop
let rec forLoop body times =
    if times <= 0 then
        ()
    else
        body()
        forLoop body (times - 1)

// Functional while loop
let rec whileLoop predicate body =
    if predicate() then
        body()
        whileLoop predicate body
    else
        ()

forLoop (fun () -> printfn "Looping...") 3

// Simulate the typical work week
open System

whileLoop
    (fun () -> DateTime.Now.DayOfWeek <> DayOfWeek.Saturday)
    (fun () -> printfn "I wish it were the weekend...")
    
// ----------------------------------------------------------------------------

// Define mutually recursive functions
let rec isOdd  n = 
    if   n = 0 then false 
    elif n = 1 then true
    else isEven (n - 1)
and isEven n = 
    if   n = 0 then true 
    elif n = 1 then false
    else isOdd (n - 1)

// ----------------------------------------------------------------------------

// Define mutually recursive functions
let rec isOdd  n = 
    if   n = 0 then false 
    elif n = 1 then true
    else isEven (n - 1)
and isEven n = 
    if   n = 0 then true 
    elif n = 1 then false
    else isOdd (n - 1)

// ----------------------------------------------------------------------------

let rec (!) x =
    if x <= 1 then 1
    else x * !(x - 1)

// Define (===) to compare strings based on regular expressions
open System.Text.RegularExpressions;;

let (===) str (regex : string) = Regex.Match(str, regex).Success

List.fold (+) 0 [1 .. 10]

List.fold (*) 1 [1 .. 10]

let minus = (-)
List.fold minus 10 [3; 3; 3]

// ----------------------------------------------------------------------------

open System
open System.IO

let sizeOfFolder folder =

    // Get all files under the path
    let filesInFolder : string []  = 
        Directory.GetFiles(
            folder, "*.*", 
            SearchOption.AllDirectories)

    // Map those files to their corresponding FileInfo object
    let fileInfos : FileInfo [] = 
        Array.map 
            (fun file -> new FileInfo(file)) 
            filesInFolder

    // Map those fileInfo objects to the file's size
    let fileSizes : int64 [] = 
        Array.map 
            (fun (info : FileInfo) -> info.Length) 
            fileInfos

    // Total the file sizes
    let totalSize = Array.sum fileSizes

    // Return the total size of the files
    totalSize

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let uglySizeOfFolder folder =
    Array.sum
        (Array.map 
            (fun (info : FileInfo) -> info.Length) 
            (Array.map 
                (fun file -> new FileInfo(file)) 
                (Directory.GetFiles(
                    folder, "*.*",
                    SearchOption.AllDirectories))))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let sizeOfFolderPiped folder =

    let getFiles folder = 
        Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)
    
    let totalSize =
        folder
        |> getFiles
        |> Array.map (fun file -> new FileInfo(file)) 
        |> Array.map (fun info -> info.Length)
        |> Array.sum

    totalSize

// ----------------------------------------------------------------------------

["Pipe"; "Forward"] |> List.iter (fun s -> printfn "s has length %d" s.Length)

// ----------------------------------------------------------------------------

let sizeOfFolderComposed (*No Parameters!*) =

    let getFiles folder =
        Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)

    // The result of this expression is a function that takes
    // one parameter, which will be passed to getFiles and piped
    // through the following functions.
    getFiles
    >> Array.map (fun file -> new FileInfo(file))
    >> Array.map (fun info -> info.Length)
    >> Array.sum

// ----------------------------------------------------------------------------
    
[ [1]; []; [4;5;6]; [3;4]; []; []; []; [9] ]
|> List.filter(not << List.isEmpty);;

// ----------------------------------------------------------------------------

// Truth table for AND via pattern matching
let testAnd x y =
    match x, y with
    | true,  true  -> true
    | true,  false -> false
    | false, true  -> false
    | false, false -> false;;

// Incomplete pattern matching. OOPS! Not every letter matched
let letterIndex l = 
    match l with
    | 'a' -> 1
    | 'b' -> 2
    
// Named patterns
let greet name =
    match name with
    | "Robert"  -> printfn "Hello, Bob"
    | "William" -> printfn "Hello, Bill"
    | other -> printfn "Hello, %s" other
    
// Define a literal value
[<Literal>]
let Bill = "Bill Gates"

// Match against literal values
let greet2 name =
    match name with
    | Bill -> "Hello Bill!"
    | x    -> sprintf "Hello, %s" x

// ----------------------------------------------------------------------------

// High / Low game
open System

let highLowGame () =

    let rng = new Random()
    let secretNumber = rng.Next() % 100

    let rec highLowGameStep () =
    
        printfn "Guess the secret number: "
        let guessStr = Console.ReadLine()
        let guess = Int32.Parse(guessStr)

        match guess with
        | _ when guess > secretNumber
            -> printfn "The secret number is lower."
               highLowGameStep()

        | _ when guess = secretNumber
            -> printfn "You've guessed correctly!"
               ()

        | _ when guess < secretNumber
            -> printfn "The secret number is higher."
               highLowGameStep()

    // Begin the game
    highLowGameStep()

// ----------------------------------------------------------------------------

let testXor x y =
    match x, y with
    | tuple when fst tuple <> snd tuple
        -> true
    | true,  true  -> false
    | false, false -> false

if testXor true true  <> false then failwith ""
if testXor true false <> true then failwith ""
if testXor false true <> true then failwith ""
if testXor false false <> false then failwith ""


let rec listLength l =
    match l with
    | []         -> 0
    | [_]        -> 1
    | [_; _]     -> 2
    | [_; _; _]  -> 3
    | hd :: tail -> 1 + listLength tail

let describeOption o =
    match o with
    | Some(42) -> "The answer was 42, but what was the question?"
    | Some(x)  -> sprintf "The answer was %d" x
    | None     -> "No answer found."

// ----------------------------------------------------------------------------

let x, y = (100, 200)

let addOptionValues = fun (Some(x), Some(y)) -> x + y

// The 'function' keyword
let rec funListLength = 
   function
   | []         -> 0
   | [_]        -> 1
   | [_; _]     -> 2
   | [_; _; _]  -> 3
   | hd :: tail -> 1 + funListLength tail

// ----------------------------------------------------------------------------

// Discriminated union for a card's suit
type Suit =
  | Heart
  | Diamond
  | Spade
  | Club

// Discriminated union for playing cards
type PlayingCard =
    | Ace   of Suit
    | King  of Suit
    | Queen of Suit
    | Jack  of Suit
    | ValueCard of int * Suit

// Use a list comprehension to generate a deck of cards
let deckOfCards =
    [ for suit in [ Spade; Club; Heart; Diamond ] do
          yield Ace(suit)
          yield King(suit)
          yield Queen(suit)
          yield Jack(suit)
          for value in 2 .. 10 do
              yield ValueCard(value, suit) ]

// ----------------------------------------------------------------------------

// Program statements
type Statement =
    | Print    of string
    | Sequence of Statement * Statement
    | IfStmt   of Expression * Statement * Statement

// Program expressions
and Expression =
    | Integer     of int
    | LessThan    of Expression * Expression
    | GreaterThan of Expression * Expression

(*
    if (3 > 1)
        print "3 is greater than 1"
    else
        print "3 is not"
        print "greater than 1"
*)

let program =
    IfStmt(
        GreaterThan(
            Integer(3),
            Integer(1)),
        Print("3 is greater than 1"),
        Sequence(
            Print("3 is not"),
            Print("greater than 1")
        )
    )
    
// ----------------------------------------------------------------------------
    
type BinaryTree =
     | Node of int * BinaryTree * BinaryTree
     | Empty

let rec printInOrder tree =
    match tree with
    | Node (data, left, right)
        -> printInOrder left
           printfn "Node %d" data
           printInOrder right
    | Empty
        -> ()

(*
          2
        /   \
       1     4
           /   \
          3     5
*)

let binTree = 
    Node(2,
        Node(1, Empty, Empty),
        Node(4, 
            Node(3, Empty, Empty),
            Node(5, Empty, Empty)
        )
    )
    
// ----------------------------------------------------------------------------
    
// Describe a pair of cards in a game of poker
let describeHoleCards cards =
    match cards with
    | [] 
    | [_]
        -> failwith "Too few cards."
    | cards when List.length cards > 2
        -> failwith "Too many cards."
    
    | [ Ace(_);  Ace(_)  ] -> "Pocket Rockets"
    | [ King(_); King(_) ] -> "Cowboys"
    
    | [ ValueCard(2, _); ValueCard(2, _)] 
        -> "Ducks"

    | [ Queen(_); Queen(_) ]
    | [ Jack(_);  Jack(_)  ]
        -> "Pair of face cards"
    
    | [ ValueCard(x, _); ValueCard(y, _) ] when x = y
        -> "A Pair"
    
    | [ first; second ]
        -> sprintf "Two cards: %A and %A" first second

// ----------------------------------------------------------------------------

type Employee =
    | Manager of string * Employee list
    | Worker  of string
    
let rec printOrganization worker =
    match worker with
    | Worker(name) -> printfn "Employee %s" name
    
    | Manager(managerName, [ Worker(employeeName) ] )
        -> printfn "Manager %s with Worker %s" managerName employeeName

    | Manager(managerName, [ Worker(employee1); Worker(employee2) ] )
        -> printfn 
               "Manager %s with two workers %s and %s" 
               managerName employee1 employee2
        
    | Manager(managerName, workers)
        -> printfn "Manager %s with workers..." managerName
           workers |> List.iter printOrganization

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let company = Manager("Tom", [ Worker("Pam"); Worker("Stuart") ] )
printOrganization company

// ----------------------------------------------------------------------------

// OOPS! Forgot about the Ace union case...
let getCardValue card =
    match card with
    // | Ace(_) -> 11
    | King(_) | Queen(_) | Jack(_) -> 10
    | ValueCard(x, _)              -> x

type PlayingCard2 =
    | Ace   of Suit
    | King  of Suit
    | Queen of Suit
    | Jack  of Suit
    | ValueCard of int * Suit
    member this.Value =
        match this with
        | Ace(_) -> 11
        | King(_) | Queen (_)| Jack(_)  -> 10
        | ValueCard(x, _) when x <= 10 && x >= 2 
                       -> x
        | ValueCard(_) -> failwith "Card has an invalid value!"

let highCard = Ace(Spade)
let highCardValue = highCard.Value

// ----------------------------------------------------------------------------

type Person =
    | Person of string * string * int
    
let steve = Person("Steve", "Holt", 3)
let gob = Person("Bluth", "George", 3)

// Define a record type
type PersonRec = { First : string; Last : string; Age : int};;

// Construct a record
let steve2 = { First = "Steve"; Last = "Holt"; Age = 17 };;

// Use '.field' to access record fields
printfn "%s is %d years old" steve2.First steve2.Age;;

// ----------------------------------------------------------------------------

type Car = 
    {
        Make  : string
        Model : string
        Year  : int
    }
    
let thisYear's = { Make = "FSharp"; Model = "Luxury Sedan"; Year = 2010  }
let nextYear's = { thisYear's with Year = 2011 }

// ----------------------------------------------------------------------------

let allNewCars = [thisYear's; nextYear's]

let allCoups = 
     allNewCars
     |> List.filter 
        (function 
          | { Model = "Coup" } -> true
          | _                  -> false)

// ----------------------------------------------------------------------------

type Point = { X : float; Y : float };;

// Distance between two points
let distance pt1 pt2 = 
    let square x = x * x
    sqrt <| square (pt1.X - pt2.X) + square (pt1.Y - pt2.Y);;

distance {X = 0.0; Y = 0.0} {X = 10.0; Y = 10.0};;

// ----------------------------------------------------------------------------

module OtherExample =

    type Point   = { X: float; Y: float;}
    type Vector3 = { X : float; Y : float; Z : float }

    // Provide a type annotation to not infer pt1 and pt2 to be Vector3
    let distance (pt1 : Point) (pt2 : Point) =  
        let square x = x * x
        sqrt <| square (pt1.X - pt2.X) + square (pt1.Y - pt2.Y)
        
    // Disambiguate a Point from the Vector type by
    // fully qualifying record fields.
    let origion  = { Point.X = 0.0; Point.Y = 0.0 }

// ----------------------------------------------------------------------------

// Add a property to record type Vector
type Vector = 
    { X : float; Y : float; Z : float }
    member this.Length = 
        sqrt <| this.X ** 2.0 + this.Y ** 2.0 + this.Z ** 2.0;;

let v = { X = 10.0; Y = 20.0; Z = 30.0 };;

v.Length;;

// ----------------------------------------------------------------------------

#endif

module otherExample = 
    // Define two lazy values
    let x = Lazy<int>.Create(fun () -> printfn "Evaluating x..."; 10)
    let y = lazy (printfn "Evaluating y..."; x.Value + x.Value)

    // Directly requesting y's value will force its evaluation
    x.Value

    // Accessing y's value again will use a cached value
    // (no side effects)
    y.Value

// ----------------------------------------------------------------------------

let seqOfNumbers = seq { 1 .. 10 }

let infiniteSeq = seq { for i = 0 to System.Int32.MaxValue do yield i }

let alphabet = seq { for c in 'A' .. 'Z' do yield c }

Seq.take 4 alphabet;;

let noisyAlphabet =
    seq {
        for c in 'A' .. 'Z' do
            printfn "Yielding %c..." c
            yield c
    }
    
// ----------------------------------------------------------------------------

// Sequence of random numbers
open System
let randomSequence = 
    seq {
        let rng = new Random()
        while true do
            yield rng.Next()
    }

// ----------------------------------------------------------------------------

// Generate the next element of the Fibonacci sequence give the previous
// two elements. To be used with Seq.unfold.
let nextFibUnder100 (a, b) =
    if a + b > 100 then
        None
    else
        let nextValue = a + b
        Some(nextValue, (nextValue, a));;

let fibsUnder100 = Seq.unfold nextFibUnder100 (0, 1) ;;

Seq.toList fibsUnder100;;

// ----------------------------------------------------------------------------

// Print odd numbers under 10
let oddsUnderN n = seq { for i in 1 .. 2 .. n do yield i }
Seq.iter (printfn "%d") (oddsUnderN 10);;


// Sequence of words (Arrays are compatible with sequences)
let words = "The quick brown fox jumped over the lazy dog".Split( [| ' ' |]);;

// Map strings to string, length tuples
words |> Seq.map (fun word -> word, word.Length);;

Seq.fold (+) 0 <| seq { 1 .. 100 };;


let allIntsSeq = seq { for i in 0 .. System.Int32.MaxValue -> i }
let allIntsSeq = seq { for i = 0 to System.Int32.MaxValue do yield i }

let allIntsSeq = [ for i = 0 to System.Int32.MaxValue do yield i ]


