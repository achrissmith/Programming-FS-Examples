module ProgrammingFS.Ch10

#r "System.Windows.Forms.dll"
#r "System.Drawing.dll"

open System
open System.IO
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

// val images : seq<string * System.Drawing.Image>
let images = 
    let myPictures = 
        Environment.GetFolderPath(Environment.SpecialFolder.MyPictures)
    
    Directory.GetFiles(myPictures, "*.JPG")
    |> Seq.map (fun filePath -> 
            Path.GetFileName(filePath), 
            Bitmap.FromFile(filePath))

// Create the data grid and put on a form
let dg = new DataGridView(Dock = DockStyle.Fill)
dg.DataSource <- new List<_>(images)

let f = new Form()
f.Controls.Add(dg)

f.ShowDialog()

// ----------------------------------------------------------------------------

/// Colorful printing for helpful diagnostics
let cprintfn c fmt = 
    Printf.kprintf
        (fun s -> 
            let orig = System.Console.ForegroundColor 
            System.Console.ForegroundColor <- c;
            System.Console.WriteLine(s)
            System.Console.ForegroundColor <- orig)
        fmt

open System

cprintfn ConsoleColor.Blue  "Hello, World! in blue!"
cprintfn ConsoleColor.Red   "... and in red!"
cprintfn ConsoleColor.Green "... and in green!"

let rotatingColors = 
    seq { 
        let i = ref 0
        let possibleColors = Enum.GetValues(typeof<ConsoleColor>)
        while true do
            yield (enum (!i) : ConsoleColor)
            i := (!i + 1) % possibleColors.Length
    }

"Experience the rainbow of possibility!"
|> Seq.zip rotatingColors
|> Seq.iter (fun (color, letter) -> cprintfn color "%c" letter)

// ----------------------------------------------------------------------------

open System

/// Victory!
let victory() =
    for frequency in [100 .. 50 .. 2000] do
        Console.Beep(frequency, 25)

/// Defeat :(
let defeat() =
    for frequency in [2000 .. -50 .. 37] do
        Console.Beep(frequency, 25)

// ----------------------------------------------------------------------------

open System.IO

let rec filesUnder basePath =
    seq {
        yield! Directory.GetFiles(basePath)
        for subDir in Directory.GetDirectories(basePath) do
            yield! filesUnder subDir 
    }

// ----------------------------------------------------------------------------

open System.IO

let rec filesUnder2 basePath =
    seq {
        yield! Directory.GetFiles(basePath)
        for subDir in Directory.GetDirectories(basePath) do
            yield! filesUnder subDir 
    }

let NewImageFolder = @"D:\NewImagesFolder\"

let doStuff() =
    __SOURCE_DIRECTORY__
    |> filesUnder2
    |> Seq.filter(fun filePath -> filePath.ToUpper().EndsWith("JPG"))
    |> Seq.iter(fun filePath -> let fileName = Path.GetFileName(filePath)
                                let destPath = Path.Combine(NewImageFolder, fileName)
                                File.Copy(filePath, destPath)) 

// ----------------------------------------------------------------------------

open System.Text
open System.Diagnostics

/// Spawns a new process. Returns (exit code, stdout)
let shellExecute program arguments =

    let startInfo = new ProcessStartInfo()
    startInfo.FileName  <- program
    startInfo.Arguments <- arguments
    
    startInfo.UseShellExecute        <- false
    startInfo.RedirectStandardOutput <- true

    let proc = new Process()
    proc.EnableRaisingEvents <- true
    
    // Add a handler to the 'OutputDataRecieved' event, so we can store
    // the STDOUT stream of the process.
    let driverOutput = new StringBuilder()        
    proc.OutputDataReceived.AddHandler(
        DataReceivedEventHandler(
            (fun sender args -> driverOutput.AppendLine(args.Data) |> ignore)
        )
    )
    
    proc.StartInfo <- startInfo
    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    
    proc.WaitForExit()
    (proc.ExitCode, driverOutput.ToString())

// ----------------------------------------------------------------------------

#r "stdole.dll"
#r "Microsoft.Office.Interop.Word"

open Microsoft.Office.Interop.Word

let private m_word : ApplicationClass option ref = ref None

let openWord()        = m_word := Some(new ApplicationClass())
let getWordInstance() = Option.get !m_word
let closeWord()       = (getWordInstance()).Quit()

// COM objects expect byref<obj>, ref cells will be
// converted to byref<obj> by the compiler.
let comarg x = ref (box x)

let openDocument filePath = 
    printfn "Opening %s..." filePath
    getWordInstance().Documents.Open(comarg filePath)

let printDocument (doc : Document) =
    printfn "Printing %s..." doc.Name
    
    doc.PrintOut(
        Background  = comarg true,
        Range       = comarg WdPrintOutRange.wdPrintAllDocument,
        Copies      = comarg 1, 
        PageType    = comarg WdPrintOutPages.wdPrintAllPages,
        PrintToFile = comarg false,
        Collate     = comarg true, 
        ManualDuplexPrint = comarg false,
        PrintZoomColumn = comarg 2,  // Pages 'across'
        PrintZoomRow    = comarg 2)  // Pages 'up down'

let closeDocument (doc : Document) =
    printfn "Closing %s..." doc.Name
    doc.Close(SaveChanges = comarg false)

// -------------------------------------------------------------

open System
open System.IO

let doStuff2() =
    try
        openWord()

        printfn "Printing all files in [%s]..." __SOURCE_DIRECTORY__ 

        Directory.GetFiles(__SOURCE_DIRECTORY__, "*.docx")
        |> Array.iter 
            (fun filePath -> 
                let doc = openDocument filePath
                printDocument doc
                closeDocument doc)
    finally
        closeWord()

// -------------------------------------------------------------

#r "Microsoft.Office.Interop.Excel"

open System
open System.IO
open System.Reflection
open Microsoft.Office.Interop.Excel

let app = ApplicationClass(Visible = true)

let sheet = app.Workbooks
               .Add()
               .Worksheets.[1] :?> _Worksheet

let setCellText (x : int) (y : int) (text : string) = 
    let range = sprintf "%c%d" (char (x + int 'A')) (y+1)
    sheet.Range(range).Value(Missing.Value) <- text


let printCsvToExcel rowIdx (csvText : string) =
    csvText.Split([| ',' |])
    |> Array.iteri (fun partIdx partText -> setCellText partIdx rowIdx partText)
    

let rec filesUnderFolder basePath = 
    seq {
        yield! Directory.GetFiles(basePath)
        for subFolder in Directory.GetDirectories(basePath) do
            yield! filesUnderFolder subFolder 
    }

let doStuff3() =

    // Print header
    printCsvToExcel 0 "Directory, Filename, Size, Creation Time"

    // Print rows
    filesUnderFolder (Environment.GetFolderPath(Environment.SpecialFolder.MyPictures))
    |> Seq.map (fun filename -> new FileInfo(filename))
    |> Seq.map (fun fileInfo -> sprintf "%s, %s, %d, %s" 
                                    fileInfo.DirectoryName 
                                    fileInfo.Name 
                                    fileInfo.Length 
                                    (fileInfo.CreationTime.ToShortDateString()))
    |> Seq.iteri (fun idx str -> printCsvToExcel (idx + 1) str)

