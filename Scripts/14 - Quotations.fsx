module ProgrammingFS.Ch14

// Simple addition
<@ 1 + 1 @>;;

// Lambda expression
<@@ fun x -> "Hello, " + x @@>;;

if (<@ 1 + 1 @>.Raw) = <@@ 1 + 1 @@> <> true then failwith "ERROR: Expr<_>.Raw <> Expr"

// ----------------------------------------------------------------------------

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let rec describeCode (expr : Expr) =
    match expr with
    
    // Literal value
    | Int32(i)  -> printfn "Integer with value %d" i
    | Double(f) -> printfn "Floating-point with value %f" f
    | String(s) -> printfn "String with value %s" s
    
    // Calling a method
    | Call(calledOnObject, methInfo, args)
        ->  let calledOn = match calledOnObject with
                           | Some(x) -> sprintf "%A" x
                           | None    -> "(Called a static method)"
                           
            printfn "Calling method '%s': \n\
                     On value:  %s \n\
                     With args: %A" methInfo.Name calledOn args
    
    // Lambda expressions
    | Lambda(var, lambdaBody) ->
        printfn 
            "Lambda Expression - Introduced value %s with type %s"
            var.Name var.Type.Name
        printfn "Processing body of Lambda Expression..."
        describeCode lambdaBody
    
    | _ -> printfn "Unknown expression form:\n%A" expr


// ----------------------------------------------------------------------------

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns

let describeArithmatic operation =
    match operation with
    | SpecificCall <@ (*) @> (_, _, [Int32(0); _])
    | SpecificCall <@ (*) @> (_, _, [_; Int32(0)]) ->
        printfn "Multiplication by zero."

    | SpecificCall <@ (/) @> (_, _, [lhs; Int32(0)]) ->
        printfn "Division by zero."

    | SpecificCall <@ (+) @> (_, _, [lhs; rhs]) ->
        printfn "Addition."

    | SpecificCall <@ (-) @> (_, _, [lhs; rhs]) ->
        printfn "Subtraction."
        
    | SpecificCall <@ (*) @> (_, _, [lhs; rhs]) ->
        printfn "Multiplcation."

    | SpecificCall <@ (/) @> (_, _, [lhs; rhs]) ->
        printfn "Division."
        
    | _ -> failwith "Unknown quotation form."

// ----------------------------------------------------------------------------

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let rec describeCode2 (expr : Expr) =
    match expr with
    
    // Literal value
    | Int32(i)  -> printfn "Integer literal with value %d" i
    | Double(f) -> printfn "Floating point literal with value %f" f
    | String(s) -> printfn "String literal with value %s" s
    
    // Calling a method
    | Call(calledOnObject, methInfo, args)
        ->  let calledOn = match calledOnObject with
                           | Some(x) -> sprintf "%A" x
                           | None    -> "(static method)"
                           
            printfn "Calling method '%s': \n\
                    On intance:  %s \n\
                    With args : %A" methInfo.Name calledOn args
                    
            match methInfo with
            | MethodWithReflectedDefinition(methBody) -> 
                printfn 
                    "Expanding method body of '%s'..." methInfo.Name
                describeCode2 methBody
            | _ ->  
                printfn 
                    "Unable to expand body of '%s'. Quotation stops here."
                    methInfo.Name
    
    // Lambda expressions
    | Lambda(var, lambdaBody) ->
        printfn 
            "Lambda Expression - Introducing value %s with type %s"
            var.Name var.Type.Name
        
        printfn "Processing body of Lambda Expression..."
        describeCode2 lambdaBody
    
    | _ -> printfn "Unknown expression form:\n%A" expr

// ----------------------------------------------------------------------------

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape

let rec generalizedDescribeCode indentation expr =
    
    let indentedMore = indentation + "    "
    
    match expr with
    
    // A variable being introduced
    | ShapeVar(var) ->
        printfn "%s Looking up value '%s'" indentation var.Name
    
    // The introduction of a lambda expression
    | ShapeLambda(var, lambdaBody) ->
        printfn 
            "%s Lambda expression, introducing var '%s'" 
            indentation var.Name
        generalizedDescribeCode indentedMore lambdaBody
    
    // ConstApp
    | ShapeCombination(_, exprs) ->
        printfn "%s ShapeCombination:" indentation
        exprs |> List.iter (generalizedDescribeCode indentedMore)

// ----------------------------------------------------------------------------

let rec fsharpToRpn code stackOperations =
    match code with
    | Int32(n)  -> (sprintf "Push %d" n) :: stackOperations
    | Double(f) -> (sprintf "Push %f" f) :: stackOperations

    | SpecificCall <@ (+) @> (_, _, [lhs; rhs]) ->
        let lhs = fsharpToRpn lhs stackOperations
        let rhs = fsharpToRpn rhs stackOperations
        lhs @ rhs @ ["Call (+)"]

    | SpecificCall <@ (-) @> (_, _, [lhs; rhs]) ->
        let lhs = fsharpToRpn lhs stackOperations
        let rhs = fsharpToRpn rhs stackOperations
        lhs @ rhs @ ["Call (-)"]

    | SpecificCall <@ ( * ) @> (_, _, [lhs; rhs]) ->
        let lhs = fsharpToRpn lhs stackOperations
        let rhs = fsharpToRpn rhs stackOperations
        lhs @ rhs @ ["Call (*)"]

    | SpecificCall <@ ( / ) @> (_, _, [lhs; rhs]) ->
        let lhs = fsharpToRpn lhs stackOperations
        let rhs = fsharpToRpn rhs stackOperations
        lhs @ rhs @ ["Call (/)"]

    | expr -> failwithf "Unknown Expr:\n%A" expr

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fsharpToRpn <@ 1 + 2 @> []

fsharpToRpn <@ 2 * 3 + 4 @> [];;

fsharpToRpn <@ (2 + 10) / (3 * (2 - 6 / 7 - 2)) @> []
|> List.iter (printfn "%s")

// ----------------------------------------------------------------------------

let o1 = <@ 1 @>
let s1 = Expr.Value(1)

if o1.Raw <> s1 then failwith "ERROR"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let organicQuotation = 
    <@ 
        let x = (1, 2, 3) 
        (x, x)
    @>
    
let syntheticQuotation =
    Expr.Let(
        new Var("x", typeof<int * int * int>),
        Expr.NewTuple( [ Expr.Value(1); Expr.Value(2); Expr.Value(3) ] ),
        Expr.NewTuple( [ Expr.GlobalVar("x").Raw; Expr.GlobalVar("x").Raw ] )
    )

// ----------------------------------------------------------------------------

let addTwoQuotations x y = <@ %x + %y @>

addTwoQuotations <@ 1 @> <@ 2 @>
|> describeCode2

addTwoQuotations <@ "a string".Length @> <@ (2 * 2) @>
|> describeCode2

// ----------------------------------------------------------------------------

#r "System.Core.dll"
#r "FSharp.PowerPack.dll"
#r "FSharp.PowerPack.Linq.dll"

// Adds extension methods to Expr<_>
open Microsoft.FSharp.Linq.QuotationEvaluation

// Evaluate a simple expression
let x = <@ 1 + 2 * 3 @>
x.Eval()

// Compile a function value expression
let toUpperQuotation = <@ (fun (x : string) -> x.ToUpper()) @>
let toUpperFunc = toUpperQuotation.Compile() ()

toUpperFunc "don't panic"

// ----------------------------------------------------------------------------

#r "System.Core.dll"
#r "FSharp.PowerPack.dll"
#r "FSharp.PowerPack.Linq.dll"

// Adds extension methods to Expr<_>
open Microsoft.FSharp.Linq.QuotationEvaluation

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

// Get the 'MethodInfo' object corresponding to basic arithmatic functions.
// These will be used to generate new Expr<_>.
type Operations =
    static member Add (x, y) : float = x + y
    static member Sub (x, y) : float = x - y
    static member Mul (x, y) : float = x * y
    static member Div (x, y) : float = x / y

let addMi = (typeof<Operations>).GetMethod("Add")
let subMi = (typeof<Operations>).GetMethod("Sub")
let mulMi = (typeof<Operations>).GetMethod("Mul")
let divMi = (typeof<Operations>).GetMethod("Div")
    
let rec generateDerivative (equation : Expr) =

    match equation with
    
    // Lamda - Begining of a function
    | Lambda(arg, body) -> 
        Expr.Lambda(arg, generateDerivative body)
    
    // Method Call - Begining of a function
    | Call(None, MethodWithReflectedDefinition(methBody), [ arg ]) ->
        generateDerivative methBody
    
    // Property Getter - For module-bound properties
    | PropGet(None, PropertyGetterWithReflectedDefinition(body), []) ->
        generateDerivative body
    
    // Addition
    // [d/dx] f(x) + g(x) = f'(x) + g'(x)
    | SpecificCall <@ (+) @> (_ ,[f; g]) ->
        let f' = generateDerivative f 
        let g' = generateDerivative g 
        Expr.Call(addMi, [f'; g'])

    // Subtraction
    // [d/dx] f(x) - g(x) = f'(x) - g'(x)
    | SpecificCall <@ (-) @> (_,[f; g]) ->
        let f' = generateDerivative f 
        let g' = generateDerivative g 
        Expr.Call(subMi, [f'; g'])

    // Product Rule
    // [d/dx] f(x) * g(x) = (f'(x) * g(x)) + (f(x) * g'(x))
    | SpecificCall <@ ( * ) @> (_,[f; g]) ->
        let f' = generateDerivative f 
        let g' = generateDerivative g 
        Expr.Call(addMi, 
            [ Expr.Call(mulMi, [f'; g]);
              Expr.Call(mulMi, [f; g']) ]
        )

    // Quotient Rule
    // [d/dx] f(x) / g(x) = ((f '(x) * g(x)) - (f(x) * g'(x))) / (g^2(x))
    | SpecificCall <@ ( / ) @> (_,[f; g]) ->
        let f' = generateDerivative f 
        let g' = generateDerivative g 
        
        let numerator = 
            Expr.Call(subMi, 
                [ Expr.Call(mulMi, [f'; g])
                  Expr.Call(mulMi, [f; g'])]
            )
        let denominator = Expr.Call(mulMi, [g; g])
                
        Expr.Call(divMi, [numerator; denominator])

    // Value
    // [d/dx] x = 1
    | Var(x) -> 
        Expr.Value(1.0, typeof<double>)

    // Constant
    // [d/dx] C = 0.0
    | Double(_) -> 
        Expr.Value(0.0, typeof<double>)

    | _ -> failwithf "Unrecognized Expr form: %A" equation

let f = (fun x -> 1.5*x*x*x + 3.0*x*x + -80.0*x + 5.0)

let f'  = 
    let quote : Expr = 
        generateDerivative <@ (fun x -> 1.5*x*x*x + 3.0*x*x + -80.0*x + 5.0) @>
     
    let typedQuote : Expr<float -> float> = Expr.Cast quote
     
    // Compile the Expr<_> into an actual method
    let compiledDerivative = typedQuote.Compile()
    compiledDerivative()


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

open System.IO

let generatePlot() =
    use csvFile = new StreamWriter("Plot.csv")
    csvFile.WriteLine("x, f(x), f'(x)")

    [-10.0 .. 0.1 .. 10.0]
    |> List.iter (fun x -> csvFile.WriteLine(sprintf "%f, %f, %f" x (f x) (f' x)))

    csvFile.Close()
