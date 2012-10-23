module ProgrammingFS.Ch12

[<AbstractClass>]
type Animal =
    abstract Speak : unit -> unit
    abstract NumberOfLegs : int with get

// ----------------------------------------------------------------------------

open System

type SafetyLevel =
    | RecreationalUse  = 1
    | HaveMcCoyOnDuty  = 3
    | SetPhasersToKill = 4

type HoloDeck(safetyLevel : SafetyLevel) =
    
    let mutable m_safetyLevel = safetyLevel
    
    member this.SafetyLevel with get () = m_safetyLevel
    
    [<Obsolete("Deprecated. Cannot update protocols once initialized.", true)>]
    member this.SafetyLevel with set x = m_safetyLevel <- x  

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let hd = new HoloDeck(SafetyLevel.RecreationalUse)

// Uncomment to see error.
// hd.SafetyLevel <- SafetyLevel.HaveMcCoyOnDuty

// ----------------------------------------------------------------------------

open System.Reflection

[<assembly:AssemblyDescription("RemoteCombobulator.exe")>]
[<assembly:AssemblyCompany("Initech Corporation")>]
[<assembly:AssemblyCopyright("\169 Initech Corporation.  All rights reserved.")>]
[<assembly:AssemblyProduct("Initech \174 RemoteCombobulator")>]
do()

// ----------------------------------------------------------------------------

open System.Reflection

[<
    assembly:AssemblyDescription("RemoteCombobulator.exe");
    assembly:AssemblyCompany("Initech Corporation");
    assembly:AssemblyCopyright("\169 Initech Corporation.  All rights reserved.");
    assembly:AssemblyProduct("Initech \174 RemoteCombobulator")
>]
do()

// ----------------------------------------------------------------------------

open System

/// Provide a description for a given class
[<AttributeUsage(AttributeTargets.Class)>]
type ClassDescriptionAttribute(desc) =
    inherit Attribute()
    member this.Description = desc

/// Provide a description for a given method
[<AttributeUsage(AttributeTargets.Method)>]
type MethodDescriptionAttribute(desc) =
    inherit Attribute()
    member this.Description = desc

type Widget = 
    | RedWidget 
    | GreenWidget
    | BlueWidget

/// XML Doc comments like this one are great for describing a class,
/// but are only available at compile-time. Metadata encoded into
/// attributes is available at run-time.
[<ClassDescription ("Represents a stack of Widgets.")>]
type WidgetStack() =

    let mutable m_widgets : Widget list = []

    [<MethodDescription("Pushes a new Widget onto the stack.")>]
    member this.Push(x) = m_widgets <- x :: m_widgets
    
    [<MethodDescription("Access the top of the Widget stack.")>]
    member this.Peek() = List.hd m_widgets
    
    [<MethodDescription("Pops the top Widget off the stack.")>]
    member this.Pop() = let top = List.hd m_widgets
                        m_widgets <- List.tl m_widgets
                        top

// ----------------------------------------------------------------------------

// Simple sprocket type
type Sprocket() =
    member this.Gears = 16
    member this.SerialNumber = "WJM-0520";;

// Get type information
let type1 = typeof<Sprocket>

let aSprocket = new Sprocket()
let type2 = aSprocket.GetType();;

type1 = type2;;

type1.Name;;

// ----------------------------------------------------------------------------

let fixedSeq = typeof< seq<float> >;;

let genSeq = typeof< seq<'a> >;;

let fullyGenSeq = typedefof< seq<'a> >;;

// ----------------------------------------------------------------------------

open System
open System.Reflection

/// Prints the methods, properties, and fields of a type to the console
let describeType (ty : Type) =
    
    let bindingFlags = 
        BindingFlags.Public   ||| BindingFlags.NonPublic |||
        BindingFlags.Instance ||| BindingFlags.Static    |||
        BindingFlags.DeclaredOnly 
    
    let methods = 
        ty.GetMethods(bindingFlags) 
        |> Array.fold (fun desc meth -> desc + sprintf " %s" meth.Name) ""
       
    let props = 
        ty.GetProperties(bindingFlags)
        |> Array.fold (fun desc prop -> desc + sprintf " %s" prop.Name) ""

    let fields =
        ty.GetFields(bindingFlags)
        |> Array.fold (fun desc field -> desc + sprintf " %s" field.Name) ""

    printfn "Name: %s" ty.Name
    printfn "Methods:    \n\t%s\n" methods
    printfn "Properties: \n\t%s\n" props
    printfn "Fields:     \n\t%s" fields

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

describeType (typeof<string>);;

// ----------------------------------------------------------------------------

/// Unit of measure for millileter
[<Measure>]
type ml 

type Cog =

    val mutable private oilLevel : float<ml>

    new() = { oilLevel = 100.0<ml> }

    member private this.Gears = 16
    member this.SerialNumber = "1138-THX"

    member this.Rotate() =
        // Rotating loses a bit of oil on each turn...
        this.oilLevel <- this.oilLevel - 0.01<ml>

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

describeType (typeof<string>);;

// ----------------------------------------------------------------------------

open System

// Displays data attributed with the MethodDesc or ClassDesc attributes
let printDocumentation(ty : Type) =

    // Return if a object has a given type
    let objHasType ty obj = (obj.GetType() = ty)

    let classDescription : string option = 
        ty.GetCustomAttributes(false)
        |> Seq.tryFind(objHasType typeof<ClassDescriptionAttribute>)
        |> Option.map(fun attr -> (attr :?> ClassDescriptionAttribute))
        |> Option.map(fun cda -> cda.Description)
    
    let methodDescriptions : seq<string * string option> =
        ty.GetMethods()
        |> Seq.map(fun mi -> mi, mi.GetCustomAttributes(false))
        |> Seq.map(fun (methodInfo, methodAttributes) ->
            let attributeDescription =
                methodAttributes
                |> Seq.tryFind(objHasType typeof<MethodDescriptionAttribute>)
                |> Option.map(fun atr -> (atr :?> MethodDescriptionAttribute))
                |> Option.map(fun mda -> mda.Description)
            methodInfo.Name, attributeDescription)
    
    let getDescription = function
                         | Some(desc) -> desc
                         | None       -> "(no description provided)"
    
    printfn "Info for class: %s" ty.Name
    printfn "Class Description:\n\t%s" (getDescription classDescription)
    printfn "Method Descriptions:"
    
    methodDescriptions 
    |> Seq.iter(fun (methName, desc) -> printfn 
                                            "\t%15s - %s" 
                                            methName 
                                            (getDescription desc))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

printDocumentation (typeof<WidgetStack>);;

// ----------------------------------------------------------------------------

type Suit =
  | Club
  | Diamond
  | Heart
  | Spade

type PlayingCard =
  | Ace   of Suit
  | King  of Suit
  | Queen of Suit
  | Jack  of Suit
  | ValueCard of int * Suit
  | Joker

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

describeType (typeof<PlayingCard>);;

// ----------------------------------------------------------------------------

let xenon = ("Xe", 54);;

// Get the tuple's element's types
open Microsoft.FSharp.Reflection
let tupleElementTypes = FSharpType.GetTupleElements (xenon.GetType());;

// Reflect the PlayingCard discriminated union type
FSharpType.GetUnionCases typeof<PlayingCard>
|> Array.iter (fun unionCase -> printfn "%s" unionCase.Name);;

// Type definitions
[<Measure>]
type far // Degrees fahrenheit

type Outlook = Sunny | Cloudy | Rainy

type Weather = { Outlook : Outlook; High : float<far>; Low : float<far> };;

// Reflect over the Weather record type 
FSharpType.GetRecordFields typeof<Weather>
|> Array.iter (fun propInfo -> printfn
                                   "Name [%s], Type [%s]"
                                   propInfo.Name propInfo.PropertyType.Name);;


// ----------------------------------------------------------------------------

// Definte a polite text writer
open System.IO

type PoliteWriter(stream : TextWriter) =

    member this.WriteLine(msg : string) =
        sprintf "%s... please" msg
        |> stream.WriteLine;;

// Dynamically create an instance of that class
let politeConsole =
    Activator.CreateInstance(typeof<PoliteWriter>, [| box Console.Out |]);;

(politeConsole :?> PoliteWriter).WriteLine("Hello, World!");;

// ----------------------------------------------------------------------------

FSharpValue.MakeTuple;;
FSharpValue.MakeRecord;;
FSharpValue.MakeUnion;;

// ----------------------------------------------------------------------------

// Type for representing a book
type Book(title, author) =
    // Current page, if the book is opened
    let mutable m_currentPage : int option = None

    member this.Title  = title
    member this.Author = author
    member this.CurrentPage with get () = m_currentPage
                            and  set x  = m_currentPage <- x

    override this.ToString() =
        match m_currentPage with
        | Some(pg) -> sprintf "%s by %s, opened to page %d" title author pg
        | None     -> sprintf "%s by %s, not currently opened" title author;;

let afternoonReading = new Book("The Mythical Man Month", "Brooks");;

let currentPagePropertyInfo = typeof<Book>.GetProperty("CurrentPage");;

currentPagePropertyInfo.SetValue(afternoonReading, Some(214), [| |]);;

afternoonReading.ToString();;

// ----------------------------------------------------------------------------

// Use the Question Mark operator to check if a type
// contains a given property.
let (?) (thingey : obj) (propName : string) =
    let ty = thingey.GetType()

    match ty.GetProperty(propName) with
    | null -> false
    | _    -> true;;

// All strings have a Length property
"a string"?Length;;

// Integers don't have an IsPrime property
42?IsPrime;;

// Cast a string as an obj, works since check is dynamic
("a string" :> obj) ? Length;;

// ----------------------------------------------------------------------------
(*
module RedefineQuestion = 

// Get a property value. Notice that the return type is generic.
let (?) (thingey : obj) (propName: string) : 'a =
    let propInfo = thingey.GetType().GetProperty(propName)
    propInfo.GetValue(thingey, null) :?> 'a

// Set a property value.
let (?<-) (thingey : obj) (propName : string) (newValue : 'a) =
    let propInfo = thingey.GetType().GetProperty(propName)
    propInfo.SetValue(thingey, newValue, null);;

let book = new Book("Foundation", "Asimov");;

book?CurrentPage <- Some(14);;

let currentPage : int option = book?CurrentPage;;
*)
// ----------------------------------------------------------------------------

/// Pounds
[<Measure>]
type lb

[<Measure>]
type inches

type Container =
    | Envelope
    | Box
    | Crate

type Dimensions = 
    { Length : float<inches>; Width : float<inches>; Height : float<inches> }

[<AbstractClass>]
type ShippingItem() =
    abstract Weight : float<lb>
    abstract Dimension : Dimensions
    
// Piece of paper describing what is in the box
type ShippingManifest() =
    inherit ShippingItem()
    
    override this.Weight = 0.01<lb>
    override this.Dimension = 
                { 
                    Length = 11.0<inches>
                    Width  = 8.5<inches>
                    Height = 0.01<inches> 
                }

// Will it blend?
type Blender() =
    inherit ShippingItem()
    
    override this.Weight = 14.00<lb>
    override this.Dimension = 
                { 
                    Length = 6.0<inches>; 
                    Width  = 5.0<inches>
                    Height = 12.0<inches> 
                }

/// Partial active pattern which matches only if the input is
/// greater than its parameter.
let (|GreaterThan|_|) (param : float<'a>) input = 
    if param > input 
    then Some() 
    else None

let determineBoxToUse(item : ShippingItem) =
    
    match item.Weight, item.Dimension with
    // Heavy orders must always go into a crate
    | GreaterThan 10.0<lb>, _ 
        -> Crate

    // Large orders must always go into a crate
    | _,  { Length = GreaterThan 24.0<inches>; Width = _; Height = _} 
    | _,  { Length = _; Width = GreaterThan 24.0<inches>; Height = _}
    | _,  { Length = _; Width = _; Height = GreaterThan 24.0<inches>} 
        -> Crate

    // Beefy orders must go into a box
    | GreaterThan 2.0<lb>, _ 
        -> Box
        
    // Min dimensions for a box
    | _,  { Length = GreaterThan 10.0<inches>; Width = _; Height = _} 
    | _,  { Length = _; Width = GreaterThan 10.0<inches>; Height = _}
    | _,  { Length = _; Width = _; Height = GreaterThan 10.0<inches>} 
        -> Box

    // Looks like an envelope will do
    | _ -> Envelope

// ----------------------------------------------------------------------------

open System

type FragileAttribute() = 
    inherit System.Attribute()

type FlammableAttribute() = 
    inherit System.Attribute()
    
type LiveAnimalAttribute() = 
    inherit System.Attribute()


/// A real, live wombat delivered right to your door!
[<LiveAnimal>]
type Wombat() =
    inherit ShippingItem()
    
    override this.Weight = 60.0<lb>
    override this.Dimension =
                { 
                    Length = 39.0<inches>
                    Width  = 10.0<inches>
                    Height = 13.0<inches>
                }
    override this.ToString() = "A cuddly Wombat"

[<Fragile; Flammable>]
type Fireworks() =
    inherit ShippingItem()

    override this.Weight = 5.0<lb>
    override this.Dimension =
                {
                    Length = 10.0<inches>
                    Width  = 8.0<inches>
                    Height = 5.0<inches>
                }
    override this.ToString() = "Fireworks"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#r "System.Core.dll"
open System.Collections.Generic

type ShippingRequirements =
    | NeedInsurance  of ShippingItem
    | NeedSignature  of ShippingItem
    | NeedBubbleWrap of ShippingItem

/// Get any additional requirements for shipping the package
let getShippingRequirements (contents : ShippingItem list) =
    
    let containsAttribute (targetAttrib : Type) x =
        x.GetType().GetCustomAttributes(false)
        |> Array.tryFind(fun attr -> attr.GetType() = targetAttrib)
        |> Option.isSome
                
    let itemsWithAttribute attr = 
        contents |> List.filter (containsAttribute attr)

    let requirements = new HashSet<ShippingRequirements>()
    
    // Include fragile items
    itemsWithAttribute typeof<FragileAttribute>
    |> List.iter (fun item -> requirements.Add(NeedBubbleWrap(item)) |> ignore)
    
    // Include flammable items
    itemsWithAttribute typeof<FlammableAttribute>
    |> List.iter (fun item -> requirements.Add(NeedInsurance(item)) |> ignore)
        
    // Include live animals
    itemsWithAttribute typeof<LiveAnimalAttribute>
    |> List.iter (fun item -> requirements.Add(NeedSignature(item)) |> ignore)
    
    // Return the list of special shipping requirements 
    Seq.to_list requirements
    
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getShippingRequirements [ new Fireworks() ];;

getShippingRequirements [ new Fireworks(); new Wombat() ];;

// ----------------------------------------------------------------------------

// Load the assembly
open System.Reflection
let loadAsm (name : string) = Assembly.Load(name)

// Prints assembly info to the console
let printAssemblyInfo name =
    let asm = loadAsm name
    printfn "Assembly %s has %d types" name (asm.GetTypes().Length);;
    
// Some common assemblies
[ "System"; "System.Core"; "FSharp.Core"; "System.Windows.Forms" ]
|> List.iter printAssemblyInfo;;
