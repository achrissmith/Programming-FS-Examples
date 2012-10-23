module ProgrammingFS.ApdxB

// HumanResources.fs - F# code declaring a discrimianted union

// COMMENTED OUT: namespace Initech.HumanResources

type PersonalInfo = { Name : string; EmployeeID : int }

type Employee =
    | Manager of PersonalInfo * Employee list
    | Worker  of PersonalInfo
    | Intern

// ----------------------------------------------------------------------------

// Using the System.Action type
#r "System.Core.dll"

open System

let fsAction = new Action<string>(fun arg -> printfn "Hello, %s" arg)

// Using the System.Func type
let fsFunc = new Func<string, int>(fun s -> Int32.Parse(s) * Int32.Parse(s))

// ----------------------------------------------------------------------------

(*
    public class CSharpFeatures
    {
        public static bool ByrefParams(ref int x, ref int y)
        {
            x = x * x;
            y = y * y;
            return true;
        }

        public static bool OutParams(out int x, out int y)
        {
            x = 10;
            y = 32;
            return true;
        }
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
> // Declare ref / out parameter values
let x = ref 3
let mutable y = 4;;

val x : int ref = {contents = 3;}
val mutable y : int = 4

> CSharpFeatures.ByrefParams(x, &y);;
val it : bool = true
> x;;
val it : int ref = {contents = 9;}
> y;;
val it : int = 16

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
> // Declare ref / out parameter values
let x = ref 0
let mutable y = 0;;

val x : int ref = {contents = 0;}
val mutable y : int = 0

> // Pass in both out parameters, return value is bool
CSharpFeatures.OutParams(x, &y);;
val it : bool = true
> // Pass in just one out parameter, return value is bool * int
CSharpFeatures.OutParams(x);;
val it : bool * int = (true, 32)
> // Pass in no out parameters, return value is bool * int * int
CSharpFeatures.OutParams();;
val it : bool * int * int = (true, 10, 32)

*)

// ----------------------------------------------------------------------------

// Define two classes, one with [<AllowNullLiteral>] and one without
type Widget() =
    override this.ToString() = "A Widget"

[<AllowNullLiteral>]
type Sprocket() =
    override this.ToString() = "A Sprocket"

let x : Sprocket = null

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let nullWidget   : Widget   = null
let nullSprocket : Sprocket = null

// ----------------------------------------------------------------------------

open System.Runtime.InteropServices

/// PInvoke signature for CopyFile
[<DllImport("kernel32.dll", EntryPoint="CopyFile")>]
extern bool copyfile(char[] lpExistingFile, char[] lpNewFile, bool bFailIfExists);

let file1 = @"D:\File.dat"
let file2 = @"D:\Backups\File.dat"

// Calls the CopyFile method defined in kernel32.dll
copyfile(file1.ToCharArray(), file2.ToCharArray(), false)

// ----------------------------------------------------------------------------

open System.Runtime.InteropServices

/// Define a Point using the sequential layout
[<Struct; StructLayout(LayoutKind.Sequential)>]
type SequentialPoint = 
    
    new (x, y) = { X = x; Y = y } 
    
    val X : int
    val Y : int

/// Define a rectangle struct explicitly
[<Struct; StructLayout(LayoutKind.Explicit)>]
type ExplicitRect = 
    
   new (l, r, t, b) = { left = l; top = t; right = r; bottom = b }
   
   [<FieldOffset(12)>]
   val mutable bottom : int
   [<FieldOffset(0)>]
   val mutable left : int
   [<FieldOffset(8)>]
   val mutable right : int
   [<FieldOffset(4)>]
   val mutable top : int

/// P/Invoke signature for PtInRect
[<DllImport("User32.dll", EntryPoint="PtInRect")>]
extern bool PointInRect(ExplicitRect &rect, SequentialPoint pt);
