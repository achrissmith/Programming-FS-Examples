module ProgrammingFS.ApdxA

#r "System.Windows.Forms.dll"
#r "System.Drawing.dll"

open System.Windows.Forms

// Create a form
let form = new Form()

// Create a button
let btn = new Button(Text = "Click Me")
btn.Click.AddHandler(fun _ _ ->
    MessageBox.Show("Hello, World")
    |> ignore)

// Add the button to the form
form.Controls.Add(btn)

// Display the form
form.ShowDialog()

// ----------------------------------------------------------------------------

#r "System.Windows.Forms.dll"
#r "System.Drawing.dll"
 
open System.IO
open System.Windows.Forms

// Count the number of words in a given text file.
let countWords filePath =
    System.Threading.Thread.Sleep(2000)
    let lines = File.ReadAllText(filePath)
    let words = lines.Split([| ' ' |])
    words.Length

// The complete works of Shakesepare
let filesToProcess = Directory.GetFiles(@"D:\Mesh\Material\Presentations\NWCpp - Intro to Functional Programming\FPWithFSharp\FSharpSyntax\obj\Debug")

// Setup the WinForm
let form = new Form(Text="The Words of ShakespearShakespeare", TopMost=true, Height=130)

let wordCountText = new Label(Dock = DockStyle.Bottom)
let progress = new ProgressBar(Minimum = 0, 
                               Maximum = filesToProcess.Length - 1, 
                               Dock = DockStyle.Fill)

form.Controls.Add(progress)
form.Controls.Add(wordCountText)
form.Show()

// Begin processing files asynchronously. Once each file has been 
// processed the status of the progress bar and label will be updated.
async  {

    let totalWords = ref 0
    
    for i in 0 .. filesToProcess.Length - 1 do
        totalWords := !totalWords + (countWords filesToProcess.[i])

        // Update progress bar value and text
        progress.Value <- i
        wordCountText.Text <- sprintf "%d words counted so far..." (!totalWords)
        
} |> Async.Start

// ----------------------------------------------------------------------------

#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml.dll"

open System
open System.Windows
open System.Windows.Controls


let win = new Window(Height = 128.0, Width = 360.0)


let label = new Label()
label.FontSize <- 62.0
label.Content <- "Hello, World"

win.Content <- label

let app = new Application()
app.Run(win)

// ----------------------------------------------------------------------------

#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

/// Declare the UI in XAML
let windowXaml = "
<Window
    xmlns='http://schemas.microsoft.com/winfx/2006/xaml/presentation'
    xmlns:sys='clr-namespace:System;assembly=mscorlib'
    xmlns:x='http://schemas.microsoft.com/winfx2006/xaml' >
    
    <StackPanel>
        <TextBlock>Who do you want to say hello to?</TextBlock>
        <TextBox Name='NameTextBox'> [name goes here] </TextBox>
        <Button  Name='SayHelloButton'>Say Hello!</Button>
    </StackPanel>

</Window>
" // End string-based XAML

// Load our XAML markup
let getWindow() =
    let xamlObj = XamlReader.Parse(windowXaml)
    xamlObj :?> Window

let win = getWindow()

// Get instance of the XAML-based UI controls, and wire up event handlers
let textBox = win.FindName("NameTextBox")    :?> TextBox
let button  = win.FindName("SayHelloButton") :?> Button

button.Click.AddHandler(fun _ _ -> let msg = sprintf "Hello, %s" textBox.Text
                                   MessageBox.Show(msg) |> ignore)

let app = new Application()
app.Run(win)

// ----------------------------------------------------------------------------

// Simple regular expressions
open System.Text.RegularExpressions

let (=~=) str regex = Regex.IsMatch(str, regex);;


"Does this contain a number?" =~= "\d+";;

"Does this (42) contain a number?" =~= "\d+";;
