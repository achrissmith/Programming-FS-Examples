module ProgrammingFS.Ch05

// Overriding ToString
type PunctuationMark =
    | Period
    | Comma
    | QuestionMark
    | ExclamationPoint
    override this.ToString() =
        match this with
        | Period -> "Period (.)"
        | Comma  -> "Comma (,)"
        | QuestionMark      -> "QuestionMark (?)"
        | ExclamationPoint  -> "ExclamationPoint (!)"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type ClassType(x : int) =
    member this.Value = x
    
// Overriding Equals
type ClassType2(x : int) =
    member this.Value = x
    override this.Equals(o : obj) =
        match o with
        | :? ClassType2 as other -> (other.Value = this.Value)
        | _ -> false
    override this.GetHashCode() = x

// Referential Equality on Functional Types
[<ReferenceEquality>]
type RefDUType =
    | A of int * char
    | B
    
// ----------------------------------------------------------------------------

type Point =
    val m_x : float
    val m_y : float
    
    // Constructor 1 – Takes two parameters
    new (x, y) = { m_x = x; m_y = y }

    // Constructor 2 – Takes no parameters
    new () = { m_x = 0.0; m_y = 0.0 }

    member this.Length =
        let sqr x = x * x
        sqrt <| sqr this.m_x + sqr this.m_y
    
let p1 = new Point(1.0, 1.0)
let p2 = new Point()

// ----------------------------------------------------------------------------

open System

type Point2 =
    val m_x : float
    val m_y : float
    
    // Parse a string, e.g. "1.0, 2.0"
    new (text : string) as this = 
        // Do any required pre processing
        if text = null then
            raise <| new ArgumentException("text")
            
        let parts = text.Split([| ',' |])
        let (successX, x) = Double.TryParse(parts.[0])
        let (successY, y) = Double.TryParse(parts.[1])

        if not successX || not successY then
            raise <| new ArgumentException("text")
        // Initialize class fields
        { m_x = x; m_y = y }
        then
            // Do any post processing 
            printfn 
                "Initialized to [%f, %f]"
                this.m_x
                this.m_y

// ----------------------------------------------------------------------------

type Point3(x : float, y : float) =

    let length = 
        let sqr x = x * x
        sqrt <| sqr x + sqr y
    do printfn "Initialized to [%f, %f]" x y
    
    member this.X = x
    member this.Y = y
    member this.Length = length
    
    // Define custom constructors, these must
    // call the 'main' constructor
    new() = new Point3(0.0, 0.0)

    // Define a second constructor.    
    new(text : string) =
        if text = null then
            raise <| new ArgumentException("text")
            
        let parts = text.Split([| ',' |])
        let (successX, x) = Double.TryParse(parts.[0])
        let (successY, y) = Double.TryParse(parts.[1])
        if not successX || not successY then
            raise <| new ArgumentException("text")
        // Calls the primary constructor
        new Point3(x, y)

// ----------------------------------------------------------------------------

// Define a generic class
type Arrayify<'a>(x : 'a) =
    member this.EmptyArray : 'a[] = [| |]
    member this.ArraySize1 : 'a[] = [| x |]
    member this.ArraySize2 : 'a[] = [| x; x |]
    member this.ArraySize3 : 'a[] = [| x; x; x |]
    
// Generic dicriminated union
type GenDU<'a> =
    | Tag1 of 'a
    | Tag2 of string * 'a list
    
// Generic record type
type GenRec<'a, 'b> = { Field1 : 'a; Field2 : 'b }

// ----------------------------------------------------------------------------

open System

type Circle =
    val m_radius : float

    new(r) = { m_radius = r }
    member foo.Radius = foo.m_radius
    member bar.Area = Math.PI * bar.Radius * bar.Radius
    
// ----------------------------------------------------------------------------

// Define a WaterBottle type with two properties
[<Measure>]
type ml

type WaterBottle() =
    let mutable m_amount = 0.0<ml>

    // Read-only property
    member this.Empty = (m_amount = 0.0<ml>)

    // Read-write property
    member this.Amount with get ()     = m_amount
                       and  set newAmt = m_amount <- newAmt

// ----------------------------------------------------------------------------

open System.Windows.Forms 

// Attempt one
let f1 = new Form()
f1.Text    <- "Window Title"
f1.TopMost <- true
f1.Width   <- 640
f1.Height  <- 480
f1.ShowDialog()

// Attempt two
let f2 = new Form(Text    = "Window Title",
                  TopMost = true,
                  Width   = 640,
                  Height  = 480)
f2.ShowDialog()

// ----------------------------------------------------------------------------

type Television =

    val mutable m_channel : int
    val mutable m_turnedOn : bool

    new() = { m_channel = 3; m_turnedOn = true }

    member this.TurnOn () =
        printfn "Turning on..."
        this.m_turnedOn <- true

    member this.TurnOff () =
        printfn "Turning off..."
        this.m_turnedOn <- false

    member this.ChangeChannel (newChannel : int) =
         if this.m_turnedOn = false then 
            failwith "Cannot change channel, the TV is not on." 

         printfn "Changing channel to %d..." newChannel
         this.m_channel <- newChannel

    member this.CurrentChannel = this.m_channel

// ----------------------------------------------------------------------------

// Curryable class methods
type Adder() =
    // Curried method arguments
    member this.AddTwoParams x y = x + y
    // Normal arguments
    member this.AddTwoTupledParams (x, y) = x + y
    
// ----------------------------------------------------------------------------

type SomeClass() =
    static member StaticMethod() = 5

// Static instance
type RareType() =
    // There is only one instance of m_numLeft for all instances of RareType
    static let mutable m_numLeft = 2
    
    do  
        if m_numLeft <= 0 then 
            failwith "No more left!"
        m_numLeft <- m_numLeft - 1
        printfn "Initialized a rare type, only %d left!" m_numLeft
     
    static member NumLeft = m_numLeft
    
// ----------------------------------------------------------------------------

type BitCounter =
    
    static member CountBits (x : int16) =
        let mutable x' = x
        let mutable numBits = 0
        for i = 0 to 15 do
            numBits <- numBits + int (x' &&& 1s)
            x' <- x' >>> 1
        numBits
        
    static member CountBits (x : int) =
        let mutable x' = x
        let mutable numBits = 0
        for i = 0 to 31 do
            numBits <- numBits + int (x' &&& 1)
            x' <- x' >>> 1
        numBits
        
    static member CountBits (x : int64) =
        let mutable x' = x
        let mutable numBits = 0
        for i = 0 to 63 do
            numBits <- numBits + int (x' &&& 1L)
            x' <- x' >>> 1
        numBits

// ----------------------------------------------------------------------------

type internal Ruby private(shininess, carats) =
    
    let mutable m_size = carats
    let mutable m_shininess = shininess    

    // Polishing increases shiness but decreases size
    member this.Polish() =
        this.Size   <- this.Size - 0.1
        m_shininess <- m_shininess + 0.1

    // Public getter, private setter
    member public  this.Size with get ()      = m_size
    member private this.Size with set newSize = m_size <- newSize

    member this.Shininess = m_shininess

    public new() =
        let rng = new Random()    
        let s = float (rng.Next() % 100) * 0.01
        let c = float (rng.Next() % 16) + 0.1
        new Ruby(s, c)            

    public new(carats) =
        let rng = new Random()    
        let s = float (rng.Next() % 100) * 0.01
        new Ruby(s, carats)            

// ----------------------------------------------------------------------------

type Class1AccessTest =
    val m_field : int
    member this.Value = this.m_field
    new() = { m_field = 0 }
    
type Class2AccessTest() =
    let m_field = 0
    member this.Value = m_field
    
let t1 = new Class1AccessTest()
let t2 = new Class2AccessTest()
 
// t1.$ and t2.$ shouldn't show m_field

// ----------------------------------------------------------------------------

open System.IO
open System.Collections.Generic

module Logger =

    let mutable private m_filesToWriteTo = new List<string>()
        
    let AddLogFile(filePath) = m_filesToWriteTo.Add(filePath)

    let LogMessage(message : string) = 
        for logFile in m_filesToWriteTo do
            use file = new StreamWriter(logFile, true)
            file.WriteLine(message)
            file.Close()

// ----------------------------------------------------------------------------
#if FALSE
type BLTSandwich() =
    member this.Ingredients = ["Bacon"; "Lettuce"; "Tomato"]
    member this.Calories = 450
    override this.ToString() = "BLT"
    
type TurkeySwissSandwich() =
    member this.Ingredients = ["Turkey"; "Swiss"]
    member this.Calories = 330
    override this.ToString() = "Turkey and Swiss"
#endif
// ----------------------------------------------------------------------------

// Base class
type BaseClass =
    val m_field1 : int

    new(x) = { m_field1 = x }
    member this.Field1 = this.m_field1
 
// Derived class using implicit class construction
type ImplicitDerived(field1, field2) =
    inherit BaseClass(field1)
    
    let m_field2 : int = field2
    
    member this.Field2 = m_field2
    
// Dervied class using explicit class construction
type ExplicitDerived =
    inherit BaseClass
    
    val m_field2 : int
    
    new(field1, field2) = 
        { 
            inherit BaseClass(field1)
            m_field2 = field2 
        } 
        
    member this.Field2 = this.m_field2
    
// ----------------------------------------------------------------------------

type Sandwich() =
    abstract Ingredients : string list
    default this.Ingredients = []

    abstract Calories : int
    default this.Calories = 0
    
type BLTSandwich() =
    inherit Sandwich()
    
    override this.Ingredients = ["Bacon"; "Lettuce"; "Tomato"]
    override this.Calories   = 330
    
type TurkeySwissSandwich() =
    inherit Sandwich()
    
    override this.Ingredients = ["Turkey"; "Swiss"]
    override this.Calories = 330

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// BLT with pickles 
type BLTWithPickleSandwich() =
    inherit BLTSandwich()
    
    override this.Ingredients = "Pickles" :: base.Ingredients
    override this.Calories   = 50 + base.Calories
    
// ----------------------------------------------------------------------------

[<AbstractClass>]
type Animal() =
    abstract member Legs : int
    
[<AbstractClass>]
type Dog() =
    inherit Animal()
    
    abstract member Description : string
    override this.Legs = 4
    
type Pomeranian() =
    inherit Dog()
    
    override this.Description = "Furry"
    
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let steve = new Pomeranian()
    
// Cast steve as various types
let steveAsDog    = steve :> Dog
let steveAsAnimal = steve :> Animal
let steveAsObject = steve :> obj

// ----------------------------------------------------------------------------

let whatIs (x : obj) =
    match x with
    | :? string    as s -> printfn "x is a string \"%s\"" s
    | :? int       as i -> printfn "x is an int %d" i
    | :? list<int> as l -> printfn "x is an int list '%A'" l
    | _ -> printfn "x is an '%s'" <| x.GetType().Name
    