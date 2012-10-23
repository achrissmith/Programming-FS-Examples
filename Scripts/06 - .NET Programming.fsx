module ProgrammingFS.Ch06

#if FALSE
// Implementing IDisposable
open System
open System.IO
open System.Collections.Generic

type MultiFileLogger() =
    do printfn "Constructing..."
    let m_logs = new List<StreamWriter>()

    member this.AttachLogFile file =
        let newLogFile = new StreamWriter(file, true)
        m_logs.Add(newLogFile)

    member this.LogMessage (msg : string) =
        m_logs |> Seq.iter (fun writer -> writer.WriteLine(msg))

    interface IDisposable with
        member this.Dispose() =
            printfn "Cleaning up..."
            m_logs |> Seq.iter (fun writer -> writer.Close())
            m_logs.Clear();;

// Write some code using a MultiFileLogger
let task1() =
   use logger = new MultiFileLogger()
   // ...
   printfn "Exiting the function task1.."
   ();;

task1();;

// ----------------------------------------------------------------------------

type Tastiness =
    | Delicious
    | SoSo
    | TrySomethingElse

type IConsumable = 
    abstract Eat : unit -> unit
    abstract Tastiness : Tastiness

// Protip: Eat one of these a day
type Apple() =
    interface IConsumable with
        member this.Eat() = printfn "Tastey!"
        member this.Tastiness = Delicious

// Not that tastey, but if you are really hungry will do a bind
type CardboardBox() =
    interface IConsumable with
        member this.Eat() =  printfn "Yuck!"
        member this.Tastiness = TrySomethingElse

// ----------------------------------------------------------------------------

// Define a type inferred to be an interface
type IDoStuff =
    abstract DoThings : unit -> unit

// Define an interface explicitly
type IDoStuffToo =
    interface
        abstract member DoThings : unit -> unit
    end;;
#endif
// ----------------------------------------------------------------------------

// Inherited interfaces
type IDoStuff =
    abstract DoStuff : unit -> unit

type IDoMoreStuff =
    inherit IDoStuff

    abstract DoMoreStuff : unit -> unit;;
    
// ERROR: Doesn't implement full interface inheritance heirarchy
type Foo() =
    interface IDoMoreStuff with
        override this.DoMoreStuff() = printfn "Stuff getting done...";;
        
// Works
type Bar() =
    interface IDoStuff with
        override this.DoStuff() = printfn "Stuff getting done..."
        
    interface IDoMoreStuff with
        override this.DoMoreStuff() = printfn "More stuff getting done...";;

// ----------------------------------------------------------------------------

open System.Collections.Generic

type Person = 
    { First : string; Last : string }
    override this.ToString() = sprintf "%s, %s" this.Last this.First

let people = 
    new List<_>(  
        [|
            { First = "Jomo";  Last = "Fisher" }
            { First = "Brian"; Last = "McNamara" }
            { First = "Joe";   Last = "Pamer" } 
        |] )

let printPeople ()  =
    Seq.iter (fun person -> printfn "\t %s" (person.ToString())) people

printfn "Initial ordering:"
printPeople()

// Sort people by first name
people.Sort(
    { 
        new IComparer<Person> with
            member this.Compare(l, r) = 
                if   l.First > r.First then  1
                elif l.First = r.First then  0
                else                        -1
    } )

printfn "After sorting by first name:"
printPeople()

// Sort people by last name
people.Sort(
    { 
        new IComparer<Person> with
            member this.Compare(l, r) = 
                if   l.Last > r.Last then  1
                elif l.Last = r.Last then  0
                else                      -1
    } )

printfn "After sorting by last name:"
printPeople()

// ----------------------------------------------------------------------------

// Abstract class
[<AbstractClass>]
type Sandwich() =
    abstract Ingredients : string list
    abstract Calories : int

// Object expression for a derived class
let lunch =
    {
        new Sandwich() with
             member this.Ingredients = ["Peanutbutter"; "Jelly"]
             member this.Calories = 400
    };;

lunch.Ingredients;;

// ----------------------------------------------------------------------------
(*
namespace FSCollectionExtensions

open System.Collections.Generic

module List =
    
    /// Skips the first n elements of the list
    let rec skip n list = 
        match n, list with
        | _, []       -> []
        | 0, list     -> list
        | n, hd :: tl -> skip (n - 1) tl
        
module Seq =
    
    /// Reverse the elements in the sequence
    let rec rev (s : seq<'a>) =
        let stack = new Stack<'a>()
        s |> Seq.iter stack.Push
        seq { 
            while stack.Count > 0 do
                yield stack.Pop() 
        }
*)
// ----------------------------------------------------------------------------

type ChessPiece =
    | Empty  = 0
    | Pawn   = 1
    | Knight = 3
    | Bishop = 4
    | Rook   = 5
    | Queen  = 8
    | King   = 1000000

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Create a 2D array of the ChessPiece enumeration
let createChessBoard() =
    let board = Array2D.init 8 8 (fun _ _ -> ChessPiece.Empty)

    // Place pawns
    for i = 0 to 7 do
        board.[1,i] <- ChessPiece.Pawn
        board.[6,i] <- enum<ChessPiece> (-1 * int ChessPiece.Pawn)

    // Place black pieces in order
    [| ChessPiece.Rook; ChessPiece.Knight; ChessPiece.Bishop; ChessPiece.Queen; 
       ChessPiece.King; ChessPiece.Bishop; ChessPiece.Knight; ChessPiece.Rook |]
    |> Array.iteri(fun idx piece -> board.[0,idx] <- piece)

    // Place white pieces in order
    [| ChessPiece.Rook;  ChessPiece.Knight; ChessPiece.Bishop; ChessPiece.King; 
       ChessPiece.Queen; ChessPiece.Bishop; ChessPiece.Knight; ChessPiece.Rook |]
    |> Array.iteri(fun idx piece -> 
                        board.[7,idx] <- enum<ChessPiece> (-1 * int piece))

    // Return the board
    board

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let isPawn piece =
    match piece with
    | ChessPiece.Pawn   
        -> true
    | _ -> false

let invalidPiece = enum<ChessPiece>(42)
let materialValueOfQueen = int ChessPiece.Queen

// ----------------------------------------------------------------------------

open System

// Enumeration of flag values
type FlagsEnum =
    | OptionA = 0b0001
    | OptionB = 0b0010
    | OptionC = 0b0100
    | OptionD = 0b1000
    
let isFlagSet (enum : FlagsEnum) (flag : FlagsEnum) =
    let flagName = Enum.GetName(typeof<FlagsEnum>, flag)
    if enum &&& flag = flag  then 
        printfn "Flag [%s] is set." flagName
    else
        printfn "Flag [%s] is not set." flagName

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Check if given flags are set
let customFlags = FlagsEnum.OptionA ||| FlagsEnum.OptionC

isFlagSet customFlags FlagsEnum.OptionA
isFlagSet customFlags FlagsEnum.OptionB
isFlagSet customFlags FlagsEnum.OptionC
isFlagSet customFlags FlagsEnum.OptionD;;

// ----------------------------------------------------------------------------

[<Struct>] 
type StructPoint(x : int, y : int) =
    member this.X = x
    member this.Y = y

type StructRect(top : int16, bottom : int16,
                left : int16, right : int16) =
    struct
        member this.Top    = top
        member this.Bottom = bottom
        member this.Left   = left
        member this.Right  = right

        override this.ToString() =
            sprintf "[%d, %d, %d, %d]" top bottom left right
    end

// Define two different struct values
let x = new StructPoint(6, 20)
let y = new StructPoint(6, 20);;

x = y;;

// ----------------------------------------------------------------------------

// Struct for describing a book
[<Struct>]
type BookStruct(title : string, pages : int) =
    member this.Title = title
    member this.Pages = pages
    
    override this.ToString() =
        sprintf "Title: %A, Pages: %d" this.Title this.Pages;;

// Create an instance of the struct using the constructor
let book1 = new BookStruct("Philosopher's Stone", 309);;

// Create an instance using the default constructor
let namelessBook = new BookStruct()

// Define a struct with mutable fields
[<Struct>]
type MPoint =
    [<DefaultValue>]
    val mutable X : int
    
    [<DefaultValue>]
    val mutable Y : int
    
    override this.ToString() =
        sprintf "{%d, %d}" this.X this.Y;;

let mutable pt = new MPoint();;
pt.X <- 12
pt.Y <- 30