module ProgrammingFS.Ch13

// Sequence for producing month, day tuples
let daysOfTheYear =
    seq {
        let months =
            [
                "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; 
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"
            ]

        let daysInMonth month =
            match month with
            | "Feb"
                -> 28
            | "Apr" | "Jun" | "Sep" | "Nov"
                -> 30
            | _ -> 31

        for month in months do
            for day = 1 to daysInMonth month do
                yield (month, day)
    };;

daysOfTheYear;;

Seq.length daysOfTheYear;;

// ----------------------------------------------------------------------------

type Result = Success of float | DivByZero

let divide x y =
    match y with
    | 0.0 -> DivByZero
    | _   -> Success(x / y)
    
// Total resistance of three resistors in parallel is given
// by: 1/R_e = 1/R_1 + 1/R_2 + 1/R_3
let totalResistance r1 r2 r3 =
    let r1Result = divide 1.0 r1
    match r1Result with
    | DivByZero  
        -> DivByZero
    | Success(x) 
        -> let r2Result = divide 1.0 r2
           match r2Result with
           | DivByZero 
               -> DivByZero
           | Success(y)
               -> let r3Result = divide 1.0 r3
                  match r3Result with
                  | DivByZero 
                      -> DivByZero
                  | Success(z)
                      -> let finalResult = divide 1.0 (x + y + z)
                         finalResult


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let let_with_check result restOfComputation = 
    match result with
    | DivByZero  -> DivByZero  
    | Success(x) -> restOfComputation x
    
let totalResistance2 r1 r2 r3 = 
    let_with_check 
        (divide 1.0 r1)
        (fun x ->
            let_with_check 
                (divide 1.0 r2)
                (fun y ->
                    let_with_check
                        (divide 1.0 r3)
                        (fun z -> divide 1.0 (x + y + z))
                )
        )
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let totalResistance3 r1 r2 r3 = 
    let_with_check (divide 1.0 r1) (fun x ->
    let_with_check (divide 1.0 r2) (fun y ->
    let_with_check (divide 1.0 r3) (fun z -> 
    divide 1.0 (x + y + z) ) ) )

// ----------------------------------------------------------------------------

module Step2 =

type Result = Success of float | DivByZero

let divide x y =
    match y with
    | 0.0 -> DivByZero
    | _   -> Success(x / y)

type DefinedBuilder() =
    
    member this.Bind  ((x : Result), (rest : float -> Result)) = 
        // If the result is Success(_) then execute the
        // rest of the function. Otherwise terminate it
        // prematurely.
        match x with
        | Success(x) -> rest x
        | DivByZero  -> DivByZero

    member this.Return (x : 'a) = x
    
// Create an instance of our computation expression builder 
let defined = DefinedBuilder()

let totalResistance r1 r2 r3 =
    defined {
        let! x = divide 1.0 r1
        let! y = divide 1.0 r2
        let! z = divide 1.0 r3
        return divide 1.0 (x + y + z) 
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// De-sugared form
let totalResistance2 r1 r2 r3 = 
    defined.Bind(
        (divide 1.0 r1),
        (fun x -> 
            defined.Bind(
                (divide 1.0 r2),
                (fun y -> 
                    defined.Bind(
                        (divide 1.0 r3),
                        (fun z -> 
                            defined.Return(
                                divide 1.0 (x + y + z)
                            )
                        )
                    )
                )
            )
        )
    )

// ----------------------------------------------------------------------------

// Test YieldFrom and ReturnFrom

type YieldReturnBuilder() =
    let printany = printfn "%s %A" 
    member this.Yield x      = printany "yield"  x
    member this.YieldFrom x  = printany "yield!" x
    member this.Return x     = printany "return"  x
    member this.ReturnFrom (x : obj) = printany "return! (obj)" x
    member this.ReturnFrom (x : int) = printany "return! (int)" x
    member this.Delay f = f()
    member this.Combine(x, y) = y
    
let yrb = new YieldReturnBuilder()

yrb {
    yield 1
    yield! 2
    return 3
    return! 4
}

// ----------------------------------------------------------------------------

open System.IO

let asyncProcessFile (filePath : string) (processBytes : byte[] -> byte[]) =
    async {
        
        printfn "Processing file [%s]" (Path.GetFileName(filePath))
        
        let fileStream = new FileStream(filePath, FileMode.Open)
        let bytesToRead = int fileStream.Length
        
        let! data = fileStream.AsyncRead(bytesToRead)
        
        printfn 
            "Opened [%s], read [%d] bytes" 
            (Path.GetFileName(filePath)) 
            data.Length
        
        let data' = processBytes data
        
        let resultFile = new FileStream(filePath + ".results", FileMode.Create)
        do! resultFile.AsyncWrite(data', 0, data'.Length)
        
        printfn "Finished processing file [%s]" <| Path.GetFileName(filePath)
    } |> Async.Start

// ----------------------------------------------------------------------------

open System

// Computation expression builder which rounds bound computations
// to a fix number of significant digits.
type RoundingWorkflow(sigDigs : int) =

    let round (x : float<_>) = Math.Round(float x, sigDigs)
    
    // Due to result being constrained to type float, you can only use
    // let! against float values. (Otherwise will get a compiler error.)
    member this.Bind(result : float<_>, rest : float<_> -> float<_>) =
        let result' = round result
        rest result'
        
    member this.Return (x : float<_>) = round x

let withPrecision sigDigs = new RoundingWorkflow(sigDigs)

// Test the rounding workflow
[<Measure>]
type ft

let test = 
    withPrecision 3 {
        let! x = 2.0<ft> / 12.0
        let! y = 3.5
        return x / y 
    }

if test <> 0.048 then failwith "ERROR"

// ----------------------------------------------------------------------------
(*
type StatefulFunc<'state, 'result> = StatefulFunc of ('state -> 'result * 'state)

(*
state {
    do! OpenWebpage "www.bing.com"  // Sets the state
    do! EnterText "Jellyfish"       // Uses the state
    do! ClickButton "Search"        // Uses and Sets the state
}
*)

type html = string
type webcralerstate = string

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// do! OpenWebpage "www.Bing.com"
let step1 = 
    StatefulFunc(fun initialState -> 
        let result, updatedState = OpenWebpage "www.bing.com" initialState
        result, updatedState)

// do! EnterText
let step2 = 
    StatefulFunc(fun initialState ->
        let result, updatedState = EnterText "Jellyfish" initialState
        result, updatedState)
        
// do! ClickButton "Search"
let step3 = 
    StatefulFunc(fun initialState ->
        let result, updatedState = ClickButton "Search" initialState
        result, initialState)
        
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

StatefulFunc(fun initialState ->

    let result1, updatedState1 = OpenWebPage "www.bing.com" initialState
    
    updatedState1 |> (fun initialState ->
        let result2, updatedState2 = EnterText "Jellyfish" initialState
        
        updatedState3 |> (fun initialState ->
            let result3, updatedState3 = ClickButton "Search" initialState
            
            result3, updatedState3
        )
    )
)
    
    let result2, updatedState2 = EnterText "Jellyfish" updatedState1
    
    let result3, updatedState3 = ClickButton "Search" updatedState2
    
    result, updatedState3)
        *)
// ----------------------------------------------------------------------------

open System

// Type to represent a stateful function. A function which takes an input state and
// returns a result combined with an updated state.
type StatefulFunc<'state, 'result> = StatefulFunc of ('state -> 'result * 'state)

// Run our stateful function
let Run (StatefulFunc f) initialState = f initialState

type StateBuilder() = 

    member this.Bind(
                        result : StatefulFunc<'state, 'a>, 
                        restOfComputation : 'a -> StatefulFunc<'state, 'b>
                    ) = 
                    
        StatefulFunc(fun initialState ->
            let result, updatedState = Run result initialState
            Run (restOfComputation result) updatedState
        )
    
    member this.Combine(
                         partOne : StatefulFunc<'state, unit>, 
                         partTwo : StatefulFunc<'state, 'a>
                       ) = 
                        
        StatefulFunc(fun initialState ->
            let (), updatedState = Run partOne initialState
            Run partTwo updatedState
        )
    
    member this.Delay(
                        restOfComputation : unit -> StatefulFunc<'state, 'a>
                     ) = 

        StatefulFunc (fun initialState -> 
            Run ( restOfComputation() ) initialState
        )
    
    member this.For(
                    elements : seq<'a>, 
                    forBody : ('a -> StatefulFunc<'state, unit>)
                    ) = 
        
        StatefulFunc(fun initialState ->
            let state = ref initialState
            
            for e in elements do
                let (), updatedState = Run (forBody e) (!state)
                state := updatedState
        
            // Return unit * finalState
            (), !state
        )
        
    member this.Return(x : 'a) = 
        StatefulFunc(fun initialState -> x, initialState)
    
    member this.Using<'a, 'state, 
                      'b when 'a :> IDisposable>(
                        x : 'a,
                        restOfComputation : 'a -> StatefulFunc<'state, 'b>
                    ) = 
                        
        StatefulFunc(fun initialState ->
            try
                Run (restOfComputation x) initialState
            finally
                x.Dispose()
        )

    member this.TryFinally(
                            tryBlock : StatefulFunc<'state, 'a>, 
                            finallyBlock : unit -> unit
                          ) = 
                        
        StatefulFunc(fun initialState ->
            try
                Run tryBlock initialState
            finally
                finallyBlock()
        )
            
    
    member this.TryWith(
                        tryBlock : StatefulFunc<'state, 'a>, 
                        exnHandler : exn -> StatefulFunc<'state, 'a>
                       ) = 

        StatefulFunc(fun initialState ->
            try
                Run tryBlock initialState
            with
            | e -> 
                Run (exnHandler e) initialState
        )       
    
    member this.While(
                        predicate : unit -> bool, 
                        body : StatefulFunc<'state, unit>
                     ) = 
                
        StatefulFunc(fun initialState ->    

            let state = ref initialState
            while predicate() = true do
                let (), updatedState = Run body (!state)
                state := updatedState
                
            // Return unit * finalState
            (), !state
        )
    
    member this.Zero() = 
        StatefulFunc(fun initialState -> (), initialState)

// Declare the state workflow builder
let state = StateBuilder()

// Primitive functions for getting and setting state
let GetState          = StatefulFunc (fun state -> state, state)
let SetState newState = StatefulFunc (fun prevState -> (), newState) 

// ----------------------------------------------------------------------------

let Add x =
    state { 
        let! currentTotal, history = GetState
        do! SetState (currentTotal + x, (sprintf "Added %d" x) :: history) 
    }
    
let Subtract x =
    state {
        let! currentTotal, history = GetState
        do! SetState (currentTotal - x, (sprintf "Subtracted %d" x) :: history) 
    }
    
let Multiply x =
    state { 
        let! currentTotal, history = GetState
        do! SetState (currentTotal * x, (sprintf "Multiplied by %d" x) :: history) 
    }

let Divide x =
    state {
        let! currentTotal, history = GetState
        do! SetState (currentTotal / x, (sprintf "Divided by %d" x) :: history)
    }

// ----------------------------------------------------------------------------

// Define the StatefulFunc we will use, no need to thread
// the state parameter through each function.
let calculatorActions =
    state {
        do! Add 2
        do! Multiply 10
        do! Divide 5
        do! Subtract 8
        
        return "Finished" 
    }

// Now run our SatefulFunc passing in an intial state
let sfResult, finalState = Run calculatorActions (0, []);;