module ProgrammingFS.Ch07

[<Measure>]
type fahrenheit

let printTemperature (temp : float<fahrenheit>) =

    if   temp < 32.0<_>  then
        printfn "Below Freezing!"
    elif temp < 65.0<_>  then
        printfn "Cold"
    elif temp < 75.0<_>  then
        printfn "Just right!"
    elif temp < 100.0<_> then
        printfn "Hot!"
    else
        printfn "Scorching!"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(*
// Works
let seattle = 59.0<fahrenheit>

[<Measure>]
type celsius

let cambridge = 18.0<celsius>

> // Works
- PrintTemperature seattle;;
Cold
val it : unit = ()
> PrintTemperature cambridge;;

  PrintTemperature cambridge;;
  -----------------^^^^^^^^^^

stdin(4,18): error FS0001: Type mismatch. Expecting a
    float<fahrenheit>
but given a
    float<celsius>.
The unit of measure 'fahrenheit' does not match the unit of measure  'celsius'

*)

// ----------------------------------------------------------------------------

// Define a measure for meters
[<Measure>]
type m;;

// Multiplication, goes to meters squared
1.0<m> * 1.0<m>;;

// Division, drops unit entirely
1.0<m> / 1.0<m>;;

// Repeated division, results in 1 / meters
1.0<m> / 1.0<m> / 1.0<m>;;

// ----------------------------------------------------------------------------

// Define seconds and hertz
[<Measure>]
type s

[<Measure>]
type Hz = s ^ -1;;

// If Hz was not convertible to s, this
// would result in a compile error.
3.0<s ^ -1> = 3.0<Hz>;;

// ----------------------------------------------------------------------------

// Radians
[<Measure>]
type rads;;

let halfPI = System.Math.PI * 0.5<rads>;;

// ERROR: Cannot convert float<rads> to float.
// sin halfPI;;

sin (float halfPI);;

// ----------------------------------------------------------------------------

// Represents a point respecting the unit of measure
type Point< [<Measure>] 'u >(x : float<'u>, y : float<'u>) =

    member this.X = x
    member this.Y = y

    member this.UnitlessX = float x
    member this.UnitlessY = float y

    member this.Length =
        let sqr x = x * x
        sqrt <| sqr this.X + sqr this.Y

    override this.ToString() =
        sprintf
            "{%f, %f}"
            this.UnitlessX
            this.UnitlessY

> let p = new Point<m>(10.0<m>, 10.0<m>);;

val p : Point<m>

> p.Length;;
val it : float<m> = 14.14213562

// ----------------------------------------------------------------------------

let containsVowel (word : string) =
    let letters = word |> Set.of_seq 
    match letters with
    | _ when letters.Contains('a') || letters.Contains('e') ||
             letters.Contains('i') || letters.Contains('o') ||
             letters.Contains('u') || letters.Contains('y')
        -> true
    | _ -> false

// ----------------------------------------------------------------------------

open System.IO

// Convert a file path into its extension
let (|FileExtension|) filePath = Path.GetExtension(filePath)

let determineFileType (filePath : string) =
    match filePath with
    
    // Without active patterns
    | filePath when Path.GetExtension(filePath) = ".txt"
        -> printfn "It is a text file."
    
    // Converting the data using an active pattern
    | FileExtension ".jpg" 
    | FileExtension ".png"
    | FileExtension ".gif"
        -> printfn "It is an image file."
        
    // Binding a new value
    | FileExtension ext 
        -> printfn "Unknown file extension [%s]" ext

// ----------------------------------------------------------------------------

// Active pattern for converting strings to ints
open System
let (|ToInt|) x = Int32.Parse(x)

// Check if the input string parses as the number 4
let isFour str =
    match str with
    | ToInt 4 -> true
    | _ -> false

// ----------------------------------------------------------------------------

let (|ToBool|_|) x = 
    let success, result = Boolean.TryParse(x)
    if success then Some(result)
    else            None
    
let (|ToInt|_|) x = 
    let success, result = Int32.TryParse(x)
    if success then Some(result)
    else            None
    
let (|ToFloat|_|) x = 
    let success, result = Double.TryParse(x)
    if success then Some(result)
    else            None
    
let describeString str = 
    match str with
    | ToBool  b -> printfn "%s is a bool with value %b" str b
    | ToInt   i -> printfn "%s is an integer with value %d" str i
    | ToFloat f -> printfn "%s is a float with value %f" str f
    | _         -> printfn "%s is not a bool, int, or float" str

// ----------------------------------------------------------------------------

open System.Text.RegularExpressions

// Use a regular expression to capture three groups
let (|RegexMatch3|_|) (pattern : string) (input : string) =
    let result = Regex.Match(input, pattern)
    
    if result.Success then
        match (List.tl [ for g in result.Groups -> g.Value ]) with
        | fst :: snd :: trd :: [] 
             -> Some (fst, snd, trd)
        | [] -> failwith "Match succeeded, but no groups found.\n" +
                         "Use '(.*)' to capture groups"
        | _  -> failwith "Match succeeded, but did not find exactly three groups."
    else
        None
    
let parseTime input =
    match input with
    // Match input of the form "6/20/2008"
    | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year) 
    // Match input of the form "2004-12-8"
    | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day)
        -> Some( new DateTime(int year, int month, int day) )
    | _ -> None

// ----------------------------------------------------------------------------

open System

// This active pattern divides all strings into their various meanings.
let (|Paragraph|Sentence|Word|WhiteSpace|) (input : string) =
        let input = input.Trim()
        
        if input = "" then
            WhiteSpace
        elif input.IndexOf(".") <> -1 then
            // Paragraph contains a tuple of sentence counts and sentences.
            let sentences = input.Split([|"."|], StringSplitOptions.None)
            Paragraph (sentences.Length, sentences)
        elif input.IndexOf(" ") <> -1 then
            // Sentence contains an array of string words
            Sentence (input.Split([|" "|], StringSplitOptions.None))
        else
            // Word contains a string
            Word (input)
 
// Count the number of letters of a string by breaking it down
let rec countLetters str =
    match str with
    | WhiteSpace -> 0
    | Word x     -> x.Length
    | Sentence words
        -> words 
           |> Array.map countLetters 
           |> Array.sum
    | Paragraph (_, sentences)
        -> sentences
           |> Array.map countLetters  
           |> Array.sum
           
// ----------------------------------------------------------------------------

(*
> let (|ToUpper|) (input : string) = input.ToUpper();;

val ( |ToUpper| ) : string -> string

> let f ( ToUpper x ) = printfn "x = %s" x;;

val f : string -> unit

> f "this is lower case";;
x = THIS IS LOWER CASE
val it : unit = ()
*)

// ----------------------------------------------------------------------------

// Classify movies
let (|Action|Drama|Comedy|Documentary|Horror|Romance|) movie = Action
    // ...
    
// Specific movie qualities
let (|WonAward|_|) awardTitle movie = None
    // ...
    
// Rate the movie as a date
let goodDateMovie movie =
    match movie with
    // Matching cases of the multi-case active pattern
    | Romance
    | Comedy 

    // Using the parameterized active pattern
    | WonAward "Best Picture"
    | WonAward "Best Adaptation"
        -> true

    | _ -> false
    
// ----------------------------------------------------------------------------
    
// This example requires a reference to System.Xml.dll
#r "System.Xml.dll"

open System.Xml

// Match an XML element
let (|Elem|_|) name (inp : XmlNode) =
    if inp.Name = name then Some(inp)
    else                    None

// Get the attributes of an element
let (|Attributes|) (inp : XmlNode) = inp.Attributes

// Match a specific attribute
let (|Attr|) attrName (inp : XmlAttributeCollection) =
    match inp.GetNamedItem(attrName) with
    | null -> failwithf "Attribute %s not found" attrName
    | attr -> attr.Value

// What we are actually parsing
type Part =
    | Widget   of float
    | Sprocket of string * int
    
let ParseXmlNode element = 
    match element with
    // Parse a Widget without nesting active patterns
    | Elem "Widget" xmlElement
        -> match xmlElement with
           | Attributes xmlElementsAttributes 
               -> match xmlElementsAttributes with
                  | Attr "Diameter" diameter
                      -> Widget(float diameter)

    // Parse a Sprocket using nested active patterns
    | Elem "Sprocket" (Attributes (Attr "Model" model & Attr "SerialNumber" sn))
        -> Sprocket(model, int sn)
        
    |_ -> failwith "Unknown element"


// Load the XML Document
let xmlDoc =
    let doc = new System.Xml.XmlDocument()
    let xmlText =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
        <Parts>
            <Widget Diameter='5.0' />
            <Sprocket Model='A' SerialNumber='147' />
            <Sprocket Model='B' SerialNumber='302' />
        </Parts>
        "
    doc.LoadXml(xmlText)
    doc;;
    
// Parse each document node
xmlDoc.DocumentElement.ChildNodes
|> Seq.cast<XmlElement>
|> Seq.map ParseXmlNode;;

// ----------------------------------------------------------------------------

open System.IO
open System.Net
open System.Text.RegularExpressions

let url = @"http://oreilly.com/"

// Download the webpage
let req = WebRequest.Create(url)
let resp = req.GetResponse()
let stream = resp.GetResponseStream()
let reader = new StreamReader(stream)
let html = reader.ReadToEnd()

// Extract all images
let results = Regex.Matches(html, "<img src=\"([^\"]*)\"")
let allMatches = 
    [
        for r in results do
            for grpIdx = 1 to r.Groups.Count - 1 do
                yield r.Groups.[grpIdx].Value 
    ]

let fullyQualified = 
    allMatches 
    |> List.filter (fun url -> url.StartsWith("http://"))

// Download the images
let downloadToDisk (url : string) (filePath : string) =
    use client = new System.Net.WebClient()
    client.DownloadFile (url, filePath)
       
fullyQualified 
|> List.map(fun url -> let parts = url.Split( [| '/' |] )
                       url, parts.[parts.Length - 1])
|> List.iter(fun (url, filename) -> downloadToDisk url (@"D:\Images\" + filename))

// ----------------------------------------------------------------------------

let downloadWebpage (url : string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    reader.ReadToEnd()
    
let extractImageLinks html =
    let results = Regex.Matches(html, "<img src=\"([^\"]*)\"")
    [
        for r in results do
            for grpIdx = 1 to r.Groups.Count - 1 do
                yield r.Groups.[grpIdx].Value 
    ] |> List.filter (fun url -> url.StartsWith("http://"))
    
let downloadToDisk (url : string) (filePath : string) =
    use client = new System.Net.WebClient()
    client.DownloadFile (url, filePath)

let scrapeWebsite destPath (imageUrls : string list) =
    imageUrls
    |> List.map(fun url -> 
            let parts = url.Split( [| '/' |] )
            url, parts.[parts.Length - 1])
    |> List.iter(fun (url, filename) -> 
            downloadToDisk url (Path.Combine(destPath, filename))) 

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

downloadWebpage "http://oreilly.com/"
|> extractImageLinks
|> scrapeWebsite @"C:\Images\"

// ----------------------------------------------------------------------------

type WebScraper(url) =

    let downloadWebpage (url : string) =
        let req = WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        reader.ReadToEnd()
        
    let extractImageLinks html =
        let results = Regex.Matches(html, "<img src=\"([^\"]*)\"")
        [
            for r in results do
                for grpIdx = 1 to r.Groups.Count - 1 do
                    yield r.Groups.[grpIdx].Value 
        ] |> List.filter (fun url -> url.StartsWith("http://"))
    
    let downloadToDisk (url : string) (filePath : string) =
        use client = new System.Net.WebClient()
        client.DownloadFile (url, filePath)

    let scrapeWebsite destPath (imageUrls : string list) =       
        imageUrls
        |> List.map(fun url -> 
                let parts = url.Split( [| '/' |] )
                url, parts.[parts.Length - 1])
        |> List.iter(fun (url, filename) -> 
                downloadToDisk url (Path.Combine(destPath, filename)))
                
    // Add class fields
    let m_html   = downloadWebpage url
    let m_images = extractImageLinks html
    
    // Add class members
    member this.SaveImagesToDisk(destPath) =
        scrapeWebsite destPath m_images 

// ----------------------------------------------------------------------------

let maxValue = System.Int32.MaxValue

maxValue + 1

open Checked

maxValue + 1

// ----------------------------------------------------------------------------

open List

// ----------------------------------------------------------------------------

namespace Alpha.Bravo
        
[<AutoOpen>]
module Charlie = 
    let X = 1

// ----------------------------------------------------------------------------

let x = [7; 1; 8]

2 :: x

module two =

    let x = [1; 6; 8]
    let y = [3; 4; 2]

// ----------------------------------------------------------------------------

(* 
Removes consecutive duplicate letters. 
e.g., [1; 1; 2; 2; 3; 4] => [1; 2; 3; 4]
*)

// Slow implementation...
let removeConsecutiveDupes1 lst =

    let foldFunc acc item =
        let lastLetter, dupesRemoved = acc
        match lastLetter with
        | Some(c) when c = item  
                  -> Some(c), dupesRemoved
        | Some(c) -> Some(item), dupesRemoved @ [item]
        | None    -> Some(item), [item]

    let (_, dupesRemoved) = List.fold foldFunc (None, []) lst
    dupesRemoved
    
// Fast implementation...
let removeConsecutiveDupes2 lst = 
    let f item acc =
        match acc with
        | [] 
            -> [item]
        | hd :: tl when hd <> item 
            -> item :: acc
        | _ -> acc
    
    List.foldBack f lst []

// ----------------------------------------------------------------------------

// Creating a List<_> of 100,000 integers
open System.Collections.Generic

let createMutableList() =
    let l = new List<int>()
    for i = 0 to 100000 do
        l.Add(i)
    l;;

// Creating a list of 100,000 integers
let createImmutableList() =
    let rec createList i max =
        if i = max then 
            []
        else 
            i :: createList (i + 1) max
    createList 0 100000;;

val createImmutableList : unit -> int list

// ----------------------------------------------------------------------------

type BinTree<'a> =
    | Node of 'a * BinTree * BinTree
    | Empty

let rec iter f binTree =
    match binTree with
    | Empty -> ()
    | Node(x, l, r) ->
        f x       
        iter f l  // NOT in tail position
        iter f r  // In tail position

// ----------------------------------------------------------------------------

// Print a list in revere using a continuation
let printListRev list =
    let rec printListRevTR list cont =
        match list with
        // For an empy list, execute the continuation
        | [] -> cont()
        // For other lists, add printing the current
        // node as part of the continuation.
        | hd :: tl ->
            printListRevTR tl (fun () -> printf "%d " hd
                                         cont())

    printListRevTR list (fun () -> printfn "Done!");;

val printListRev : int list -> unit

> printListRev [1 .. 10];;
10 9 8 7 6 5 4 3 2 1 Done!
val it : unit = ()

// ----------------------------------------------------------------------------

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)
    
let iter f binTree =
    
    let rec linearize binTree cont =
        match binTree with
        | Empty -> cont()
        | Node(x, l, r) ->
            Step(x, (fun () -> linearize l (fun() -> linearize r cont)))
    
    let steps = linearize binTree (fun () -> Finished)
    
    let rec processSteps step =
        match step with
        | Finished -> ()
        | Step(x, getNext) 
            ->  f x
                processSteps (getNext())
                
    processSteps steps
    
// ----------------------------------------------------------------------------

[<Measure>]
type usd

type Entree = { Name : string; Price : float<usd>; Calories : int }

// Picks the cheapest item on the menu
let pickCheapest menuItems =
    List.reduce
        (fun acc item -> if item.Price < acc.Price 
                         then item
                         else acc) 
        menuItems
        
let pickHealthiest menuItems =
    List.reduce
        (fun acc item -> if item.Calories < acc.Calories
                         then item
                         else acc) 
        menuItems

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let private pickItem cmp menuItems = 
    let reduceFunc acc item = 
        match cmp acc item with
        | true  -> acc
        | false -> item
    List.reduce reduceFunc menuItems
    
let pickCheapest2   = pickItem (fun acc item -> item.Price < acc.Price)
let pickHealthiest2 = pickItem (fun acc item -> item.Calories < acc.Calories)

// ----------------------------------------------------------------------------

// Data type for a set. Notice the implementation is 
// stored in record fields...
type Set = 
    { 
        // Add an item to the set
        Add    : int -> Set
        // Checks if an element exists in the set
        Exists : int -> bool 
    }
    
    // Returns an empty set
    static member Empty =
        let rec makeSet lst = 
            { 
                Add    = (fun item -> makeSet (item :: lst))
                Exists = (fun item -> List.exists ((=) item) lst)
            }
        makeSet [] 

// ----------------------------------------------------------------------------

open System.Collections.Generic

let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()
    
    let memoizedFunc (input : 'a) =
        let (alreadyEvaluated, prevResult) = dict.TryGetValue(input)
        if alreadyEvaluated then
            prevResult
        else
            // Evaluate and add to lookup table 
            let answer = f input
            dict.Add(input, answer)
            answer
    
    // Return our memoized version of f dict is captured in the closure
    memoizedFunc

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Some long running function...
let f x =
   // Sleep for x seconds
   System.Threading.Thread.Sleep(x * 1000)
   // Return x
   x;;

// Benchmark
- #time;;

--> Timing now on

> someLongRunningFunc 10;;
Real: 00:00:10.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 10
> someLongRunningFunc 10;;
Real: 00:00:10.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 10
> memoizedFunc 10;;
Real: 00:00:10.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 10
> memoizedFunc 10;;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 10

// ----------------------------------------------------------------------------

// WRONG way to memoize the function - fib doesn't call the
// memoized version recursively.
let wrongMemFib =
    let rec fib x =
        match x with
        | 0 | 1 -> 1
        | 2 -> 2
        | n -> fib (n - 1) + fib (n - 2)

    memoize fib;;

// CORRECT way to memoize the function - fib  does call
// the memoized version recursively.
let rec rightMemFib =
    let fib x =
        match x with
        | 0 | 1 -> 1
        | 2 -> 2
        | n -> rightMemFib (n - 1) + rightMemFib (n - 2)

    memoize fib;;

// Benchmark the performance advantage of memoization
#time;;

> memFib 45;;
Real: 00:00:21.611, CPU: 00:00:21.247, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 1836311903
> memFib2 45;;
Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 1836311903

// ----------------------------------------------------------------------------

// Code to produce Widgets, the backbone of all .NET applications
type Widget =
    | Sprocket of string * int
    | Cog of string * float

let mutable generateWidget =
    let count = ref 0
    (fun () -> incr count
               Sprocket("Model A1", !count));;

generateWidget();;
generateWidget();;

> generateWidget <-
-     let count = ref 0
-     (fun () -> incr count
-                Cog( (sprintf "Version 0x%x" !count), 0.0));;
val it : unit = ()
> generateWidget();;
val it : Widget = Cog ("Version 0x1",0.0)
> generateWidget();;
val it : Widget = Cog ("Version 0x2",0.0)

// ----------------------------------------------------------------------------

type LazyBinTree<'a> =
    | Node of 'a * LazyBinTree<'a> Lazy * LazyBinTree<'a> Lazy
    | Empty
    
let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(x, l, r) ->
        Node(
            f x, 
            lazy(
                let lfNode = l.Value
                map f lfNode
            ), 
            lazy(
                let rtNode = r.Value
                map f rtNode
            )
        )
        
// ----------------------------------------------------------------------------

open System
open System.IO

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)
        
        // Skip header row
        fileReader.ReadLine() |> ignore
        
        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split( [| ',' |] )
    }

let rootPath = @"D:\DataFiles\"
let csvFiles = Directory.GetFiles(rootPath, "*.csv")

let allCsvData = 
    csvFiles
    |> Seq.map processFile
    |> Seq.concat

