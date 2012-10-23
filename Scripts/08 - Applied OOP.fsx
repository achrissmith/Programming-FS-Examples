module ProgrammingFS.Ch08

[<Measure>]
type ml

type Bottle(capacity : float<ml>) =

    new() = new Bottle(0.0<ml>)

    member this.Volume = capacity

    static member (+) ((lhs : Bottle), rhs) =
        new Bottle(lhs.Volume + rhs)

    static member (-) ((lhs : Bottle), rhs) =
        new Bottle(lhs.Volume - rhs)

    static member (~-) (rhs : Bottle) = 
        new Bottle(rhs.Volume * -1.0<1>)
    
    override this.ToString() =
        sprintf "Bottle(%.1fml)" (float capacity)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let half = new Bottle(500.0<ml>);;

half + 500.0<ml>;;

half - 500.0<ml>;;

-half;;

// ----------------------------------------------------------------------------

type Person =
    | Boy of string
    | Girl of string
    | Couple of Person * Person
    static member (+) (lhs, rhs) =
        match lhs, rhs with
        | Couple(_), _
        | _, Couple(_)
            -> failwith "Three's a crowd!"
        | _ -> Couple(lhs, rhs)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let testCouple = Boy("Dick") + Girl("Jane")
if testCouple <> Couple(Boy("Dick"), Girl("Jane")) then failwith "ERROR"

// ----------------------------------------------------------------------------

open System

type Year(year : int) =

    member this.Item (idx : int) =
        if idx < 1 || idx > 365 then
            failwith "Invalid day range"
        
        let dateStr = sprintf "1-1-%d" year
        DateTime.Parse(dateStr).AddDays(float (idx - 1))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Using a custom indexer
let eightyTwo   = new Year(1982)
let specialDay = eightyTwo.[171];;

specialDay.Month, specialDay.Day, specialDay.DayOfWeek;;

// ----------------------------------------------------------------------------

type Year2(year : int) =

    member this.Item (month : string, day : int) =
        let monthIdx = 
            match month.ToUpper() with
            | "JANUARY" -> 1  | "FEBRUARY" -> 2  | "MARCH"     -> 3   
            | "APRIL"   -> 4  | "MAY"      -> 5  | "JUNE"      -> 6
            | "JULY"    -> 7  | "AUGUST"   -> 8  | "SEPTEMBER" -> 9
            | "OCTOBER" -> 10 | "NOVEMBER" -> 11 | "DECEMBER"  -> 12
            | _ -> failwithf "Invalid month [%s]" month
                               
        let dateStr = sprintf "1-1-%d" year
        DateTime.Parse(dateStr).AddMonths(monthIdx - 1).AddDays(float (day - 1))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Using a non-integer index
let O'seven = new Year2(2007)
let randomDay = O'seven.["April", 7];;

randomDay.Month, randomDay.Day, randomDay.DayOfWeek;;

// ----------------------------------------------------------------------------

open System.Collections.Generic

type WordBuilder(startingLetters : string) =
    let m_letters = new List<char>(startingLetters)

    member this.Item
        with get idx   = m_letters.[idx]
        and  set idx c = m_letters.[idx] <- c

    member this.Word = new string (m_letters.ToArray())

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let wb = new WordBuilder("Jurassic Park");;

wb.Word;;

wb.[10];;

wb.[10] <- 'o';;

wb.Word;;

// ----------------------------------------------------------------------------

type TextBlock(text : string) =
    let words = text.Split([| ' ' |])
    
    member this.AverageWordLength =
        words |> Array.map float |> Array.average
    
    member this.GetSlice(lowerBound : int option, upperBound : int option) =
        let words =
            match lowerBound, upperBound with
            // Specify both upper and lower bounds
            | Some(lb), Some(ub) -> words.[lb..ub]
            // Just one bound specified
            | Some(lb), None     -> words.[lb..]
            | None,     Some(ub) -> words.[..ub]
            // No lower or upper bounds
            | None,      None -> words.[*]
            
        words

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Using a custom slice operator
let text = "The quick brown fox jumped over the lazy dog"
let tb = new TextBlock(text);;

tb.[..5];;

tb.[4..7];;

// ----------------------------------------------------------------------------

open System

type DataPoints(points : seq<float * float>) =
    member this.GetSlice(xlb, xub, ylb, yub) =

        let getValue optType defaultValue =
            match optType with
            | Some(x) -> x
            | None    -> defaultValue        

        let minX = getValue xlb Double.MinValue
        let maxX = getValue xub Double.MaxValue
        
        let minY = getValue ylb Double.MinValue
        let maxY = getValue yub Double.MaxValue

        // Return if a given tuple representing a point is within range        
        let inRange (x, y) = 
            (minX < x && x < maxX && 
             minY < y && y < maxY)

        Seq.filter inRange points 

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Define 1,000 random points with value between 0.0 to 1.0
let points = 
  seq {
      let rng = new Random()
      for i = 0 to 1000 do
          let x = rng.NextDouble()
          let y = rng.NextDouble()
          yield (x, y)
  };;

points;;

let d = new DataPoints(points);;

// Get all values where the x and y values are greater than 0.5.
d.[0.5.., 0.5..];;

// Get all values where the x-value is between 0.90 and 0.99, 
// with no restriction on the y-value.
d.[0.90 .. 0.99, *];;

// ----------------------------------------------------------------------------

open System
open System.Collections.Generic

exception NotGreaterThanHead 

/// Keep a list of items where each item added to it is
/// greater than the first element of the list.
type GreaterThanList< 'a when 'a :> IComparable<'a> >(minValue : 'a) =

    let m_head = minValue

    let m_list = new List<'a>()
    do m_list.Add(minValue)


    member this.Add(newItem : 'a) =
        // Casting to IComparable wouldn't be possible
        // if 'a weren't constrainted
        let ic = newItem :> IComparable<'a>

        if ic.CompareTo(m_head) >= 0 then
            m_list.Add(newItem)
        else
            raise NotGreaterThanHead
            
    member this.Items = m_list :> seq<'a>

// ----------------------------------------------------------------------------

open System

let compareWithNew (x : 'a when 'a : (new : unit -> 'a) and 
                        'a :> IComparable<'a>) =

    // Creating new instance of 'a because the type constraint enforces
    // there be a valid constructor.
    let clone = new 'a()

    // Comparing x with a new instance, because we know 'a implements IComparable
    let ic = x :> IComparable<'a>
    ic.CompareTo(clone)

// ----------------------------------------------------------------------------

open System.Collections.Generic

[<Measure>]
type ml

type CoffeeCup(amount : float<ml>) =
    let mutable m_amountLeft = amount
    let mutable m_interestedParties = List<(CoffeeCup -> unit)>()
    
    member this.Drink(amount) =
        printfn "Drinking %.1f..." (float amount)
        m_amountLeft <- max (m_amountLeft - amount) 0.0<ml>
        if m_amountLeft <= 0.0<ml> then
            this.LetPeopleKnowI'mEmpty()

    member this.Refil(amountAdded) =
        printfn "Coffee Cup refilled with %.1f" (float amountAdded)
        m_amountLeft <- m_amountLeft + amountAdded
            
    member this.WhenYou'reEmptyCall(func) =
        m_interestedParties.Add(func)
            
    member private this.LetPeopleKnowI'mEmpty() =
        printfn "Uh oh, I'm empty! Letting people know..."
        for interestedParty in m_interestedParties do
            interestedParty(this)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let cup = new CoffeeCup(100.0<ml>);;

// Be notified when the cup is empty
cup.WhenYou'reEmptyCall(
    (fun cup ->
        printfn "Thanks for letting me know..."
        cup.Refil(50.0<ml>)));;

cup.Drink(75.0<ml>);;

cup.Drink(75.0<ml>);;

// ----------------------------------------------------------------------------

let functionValue x y =
    printfn "x = %d, y = %d" x y
    x + y
    
// Defining a delegate
type DelegateType = delegate of int * int -> int

// Construct a delegate value
let delegateValue1 = 
    new DelegateType(
        fun x y ->
            printfn "x = %d, y = %d" x y
            x + y
    )
  
// Calling function values and delegates
let functionResult = functionValue 1 2
let delegateResult = delegateValue1.Invoke(1, 2)

// ----------------------------------------------------------------------------

type IntDelegate = delegate of int -> unit

type ListHelper =
    /// Invokes a delegate for every element of a list
    static member ApplyDelegate (l : int list, d : IntDelegate) =
        List.iter (fun x -> d.Invoke(x)) l

// Explicitly constructing the delegate    
ListHelper.ApplyDelegate([1 .. 10], new IntDelegate(fun x -> printfn "%d" x))

// Implicitly constructing the delegate
ListHelper.ApplyDelegate([1 .. 10], (fun x -> printfn "%d" x))

// ----------------------------------------------------------------------------

open System.IO

type LogMessage = delegate of string -> unit

let printToConsole  =
    LogMessage(fun msg -> printfn "Logging to console: %s..." msg)

let appendToLogFile =
    LogMessage(fun msg -> printfn "Logging to file: %s..." msg
                          use file = new StreamWriter("Log.txt", true)
                          file.WriteLine(msg))

let doBoth = LogMessage.Combine(printToConsole, appendToLogFile)
let typedDoBoth = doBoth :?> LogMessage

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

typedDoBoth.Invoke("[some important message]")

// ----------------------------------------------------------------------------

type SetAction = Added | Removed

type SetOperationEventArgs<'a>(value : 'a, action : SetAction) =
    inherit System.EventArgs()
    
    member this.Action = action
    member this.Value = value

type SetOperationDelegate<'a> = delegate of obj * SetOperationEventArgs<'a> -> unit

// Contains a set of items that fires events whenever
// items are added.
type NoisySet<'a when 'a : comparison>() =
    let mutable m_set = Set.empty : Set<'a>
    
    let m_itemAdded = 
        new Event<SetOperationDelegate<'a>, SetOperationEventArgs<'a>>()

    let m_itemRemoved = 
        new Event<SetOperationDelegate<'a>, SetOperationEventArgs<'a>>()    
    
    member this.Add(x) = 
        m_set <- m_set.Add(x)
        // Fire the 'Add' event
        m_itemAdded.Trigger(this, new SetOperationEventArgs<_>(x, Added))
        
    member this.Remove(x) =
        m_set <- m_set.Remove(x)
        // Fire the 'Remove' event
        m_itemRemoved.Trigger(this, new SetOperationEventArgs<_>(x, Removed))

    // Publish the events so others can subscribe to them
    member this.ItemAddedEvent   = m_itemAdded.Publish
    member this.ItemRemovedEvent = m_itemRemoved.Publish

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Using events
let s = new NoisySet<int>()

let setOperationHandler =
    new SetOperationDelegate<int>(
        fun sender args ->
            printfn "%d was %A" args.Value args.Action
    )

s.ItemAddedEvent.AddHandler(setOperationHandler)
s.ItemRemovedEvent.AddHandler(setOperationHandler);;

s.Add(9);;

s.Remove(9);;

// ----------------------------------------------------------------------------

open System

type ClockUpdateDelegate = delegate of int * int * int -> unit

type Clock() =

    let m_event = new DelegateEvent<ClockUpdateDelegate>()

    member this.Start() =
        printfn "Started..."
        while true do
            // Sleep one second...
            Threading.Thread.Sleep(1000)
        
            let hour   = DateTime.Now.Hour
            let minute = DateTime.Now.Minute
            let second = DateTime.Now.Second
           
            m_event.Trigger( [| box hour; box minute; box second |] )

    member this.ClockUpdate = m_event.Publish


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Non-standard event types
let c = new Clock();;

// Adding an event handler
c.ClockUpdate.AddHandler(
    new ClockUpdateDelegate(
        fun h m s -> printfn "[%d:%d:%d]" h m s
    )
);;

c.Start();;

// ----------------------------------------------------------------------------

[<Measure>]
type minute

[<Measure>]
type bpm = 1/minute

type MusicGenre = Classical | Pop | HipHop | Rock | Latin | Country

type Song = { Title : string; Genre : MusicGenre; BPM : int<bpm> }

type SongChangeArgs(title : string, genre : MusicGenre, bpm : int<bpm>) =
    inherit System.EventArgs()
    
    member this.Title = title
    member this.Genre = genre
    member this.BeatsPerMinute = bpm

type SongChangeDelegate = delegate of obj * SongChangeArgs -> unit

type JukeBox() =
    let m_songStartedEvent = new Event<SongChangeDelegate, SongChangeArgs>()
    
    member this.PlaySong(song) = 
        m_songStartedEvent.Trigger(
            this,
            new SongChangeArgs(song.Title, song.Genre, song.BPM)
        )
    
    [<CLIEvent>]
    member this.SongStartedEvent = m_songStartedEvent.Publish

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Use the Observable module to only subscribe to specific events
let jb = new JukeBox()

let fastSongEvent, slowSongEvent =
    jb.SongStartedEvent
    // Filter event to just dance music
    |> Observable.filter(fun songArgs ->
            match songArgs.Genre with
            | Pop | HipHop | Latin | Country -> true
            | _ -> false)
    // Split the event into 'fast song' and 'slow song'
    |> Observable.partition(fun songChangeArgs -> 
            songChangeArgs.BeatsPerMinute >= 120<bpm>);;

// Add event handlers to the IObservable event
slowSongEvent.Add(fun args -> printfn 
                                  "You hear '%s' and start to dance slowly..." 
                                  args.Title)

fastSongEvent.Add(fun args -> printfn 
                                  "You hear '%s' and start to dance fast!" 
                                  args.Title);;

jb.PlaySong( { Title = "Burnin Love"; Genre = Pop; BPM = 120<bpm> } );;

// ----------------------------------------------------------------------------

open System.Windows.Forms

let form = new Form(Text="Keep out of the bottom!", TopMost=true)

form.MouseMove
|> Observable.filter (fun moveArgs -> moveArgs.Y > form.Height / 2)
|> Observable.add    (fun moveArgs -> MessageBox.Show("Moved into bottom half!")
                                      |> ignore)

form.ShowDialog()

// ----------------------------------------------------------------------------

// Combine two song events
let justDanceEvent = Observable.merge slowSongEvent fastSongEvent
justDanceEvent.Add(fun args -> printfn "You start dancing, regardless of tempo!");;

// Queue up another song
jb.PlaySong( 
     { Title = "Escape (The Pina Colada Song)"; Genre = Pop; BPM = 70<bpm> } );;

// ----------------------------------------------------------------------------

// Create the form
open System.Windows.Forms

let form = new Form(Text="Relative Clicking", TopMost=true)

form.MouseClick.AddHandler(
    new MouseEventHandler(
        fun sender clickArgs ->
            printfn "MouseClickEvent    @ [%d, %d]" clickArgs.X clickArgs.Y
    )
);;

// Create a new click event relative to the center of the form
let centeredClickEvent =
    form.MouseClick
    |> Observable.map (fun clickArgs -> clickArgs.X - (form.Width  / 2),
                                        clickArgs.Y - (form.Height / 2))
// Subscribe
centeredClickEvent
|> Observable.add (fun (x, y) -> printfn "CenteredClickEvent @ [%d, %d]" x y);;

// The output is from clicking the dialog twice, first in the 
// top left corner and then in the center.
form.ShowDialog();;

// ----------------------------------------------------------------------------

open System

[<Measure>]
type ml

type EmptyCoffeeCupDelegate = delegate of obj * EventArgs ->  unit

type EventfulCoffeeCup(amount : float<ml>) =
    let mutable m_amountLeft = amount
    let m_emptyCupEvent = new Event<EmptyCoffeeCupDelegate, EventArgs>()
    
    member this.Drink(amount) =
        printfn "Drinking %.1f..." (float amount)
        m_amountLeft <- min (m_amountLeft - amount) 0.0<ml>
        if m_amountLeft <= 0.0<ml> then
            m_emptyCupEvent.Trigger(this, new EventArgs())

    member this.Refil(amountAdded) =
        printfn "Coffee Cup refilled with %.1f" (float amountAdded)
        m_amountLeft <- m_amountLeft + amountAdded
        
    [<CLIEvent>]
    member this.EmptyCup = m_emptyCupEvent.Publish
