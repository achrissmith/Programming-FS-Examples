module ProgrammingFS.Ch9

// ----------------------------------------------------------------------------

// Creating new threads
open System
open System.Threading

// What will execute on each thread
let threadBody() =
    for i in 1 .. 5 do
        // Wait 1/10 of a second
        Thread.Sleep(100)
        printfn "[Thread %d] %d..." 
            Thread.CurrentThread.ManagedThreadId
            i

let spawnThread() =
    let thread = new Thread(threadBody)
    thread.Start()
    
// Spawn a couple of threads at once
spawnThread()
spawnThread()

// ----------------------------------------------------------------------------

open System.Threading

ThreadPool.QueueUserWorkItem(fun _ -> for i = 1 to 5 do printfn "%d" i)

// Our thread pool task, note that the delegate's
// parameter is of type obj
let printNumbers (max : obj) =
    for i = 1 to (max :?> int) do
        printfn "%d" i

ThreadPool.QueueUserWorkItem(new WaitCallback(printNumbers), box 5)

// ----------------------------------------------------------------------------

// Race condition
open System.Threading

let sumArray (arr : int[]) =
    let total = ref 0

    // Add the first half
    let thread1Finished = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = 0 to arr.Length / 2 - 1 do
                    total := arr.[i] + !total
                 thread1Finished := true
        ) |> ignore

    // Add the second half
    let thread2Finished = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = arr.Length / 2 to arr.Length - 1 do
                    total := arr.[i] + !total
                 thread2Finished := true
        ) |> ignore

    // Wait while the two threads finish their work
    while !thread1Finished = false ||
          !thread2Finished = false do

          Thread.Sleep(0)

    !total

// ----------------------------------------------------------------------------

let lockedSumArray (arr : int[]) =
    let total = ref 0

    // Add the first half
    let thread1Finished = ref false
    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = 0 to arr.Length / 2 - 1 do
                    lock (total) (fun () -> total := arr.[i] + !total)
                 thread1Finished := true
        ) |> ignore

    // Add the second half
    let thread2Finished = ref false
    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = arr.Length / 2 to arr.Length - 1 do
                    lock (total) (fun () -> total := arr.[i] + !total)
                 thread2Finished := true
        ) |> ignore

    // Wait while the two threads finish their work
    while !thread1Finished = false ||
          !thread2Finished = false do

          Thread.Sleep(0)

    !total

// ----------------------------------------------------------------------------

type BankAccount = { AccountID : int; OwnerName : string; mutable Balance : int }

/// Transfer money between bank accounts
let transferFunds amount fromAcct toAcct =

    printfn "Locking %s's account to deposit funds..." toAcct.OwnerName
    lock fromAcct
        (fun () ->
            printfn "Locking %s's account to withdrawl funds..." fromAcct.OwnerName
            lock toAcct
                (fun () -> 
                    fromAcct.Balance <- fromAcct.Balance - amount
                    toAcct.Balance   <- toAcct.Balance + amount
                )
        )

// ----------------------------------------------------------------------------

open System
open System.IO

let processFileAsync (filePath : string) (processBytes : byte[] -> byte[]) =

    // This is the callback from when the AsyncWrite completes
    let asyncWriteCallback = 
        new AsyncCallback(fun (iar : IAsyncResult) ->
             // Get state from the async result
            let writeStream = iar.AsyncState :?> FileStream
            
            // End the async write operation by calling EndWrite
            let bytesWritten = writeStream.EndWrite(iar)
            writeStream.Close()
            
            printfn 
                "Finished processing file [%s]" 
                (Path.GetFileName(writeStream.Name))
        )
    
    // This is the callback from when the AsyncRead completes
    let asyncReadCallback = 
        new AsyncCallback(fun (iar : IAsyncResult) -> 
            // Get state from the async result
            let readStream, data = iar.AsyncState :?> (FileStream * byte[])
            
            // End the async read by calling EndRead
            let bytesRead = readStream.EndRead(iar)
            readStream.Close()
            
            // Process the result
            printfn 
                "Processing file [%s], read [%d] bytes" 
                (Path.GetFileName(readStream.Name))
                bytesRead
                
            let updatedBytes = processBytes data
            
            let resultFile = new FileStream(readStream.Name + ".result",
                                           FileMode.Create)
            
            let _ = 
                resultFile.BeginWrite(
                    updatedBytes, 
                    0, updatedBytes.Length, 
                    asyncWriteCallback, 
                    resultFile)
                    
            ()
        )

    // Begin the async read, whose callback will begin the async write
    let fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read, 
                                    FileShare.Read, 2048,
                                    FileOptions.Asynchronous)

    let fileLength = int fileStream.Length
    let buffer = Array.zeroCreate fileLength

    // State passed into the async read
    let state = (fileStream, buffer)
    
    printfn "Processing file [%s]" (Path.GetFileName(filePath))
    let _ = fileStream.BeginRead(buffer, 0, buffer.Length, 
                                 asyncReadCallback, state)
    ()

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Test it

// Create a text file filled entirely with 'a's
let allABytes = Array.init 1024 (fun _ -> byte 'a')

// FileToProcess should read "aaaaaaaaaaaaaaa..."
File.WriteAllBytes(@"C:\Scratch\FileToProcess.txt", allABytes)

// Now update the file's contents asynchronously
let processData fileContents =
    fileContents
    |> Array.mapi (fun idx (b : byte) -> b + byte (idx % 26))
    
// After the async operation completes, 
// FileToProcess.txt.results will read "abcdefghijklmopqrstuv..."
processFileAsync @"C:\Scratch\FileToProcess.txt" processData

// ----------------------------------------------------------------------------

open System.IO

let asyncProcessFile (filePath : string) (processBytes : byte[] -> byte[]) =
    async {
        
        printfn "Processing file [%s]" (Path.GetFileName(filePath))
        
        use fileStream = new FileStream(filePath, FileMode.Open)
        let bytesToRead = int fileStream.Length
        
        let! data = fileStream.AsyncRead(bytesToRead)
        
        printfn 
            "Opened [%s], read [%d] bytes" 
            (Path.GetFileName(filePath)) 
            data.Length
        
        let data' = processBytes data
        
        use resultFile = new FileStream(filePath + ".results", FileMode.Create)
        do! resultFile.AsyncWrite(data', 0, data'.Length)
        
        printfn "Finished processing file [%s]" <| Path.GetFileName(filePath)
    } |> Async.Start

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Test it

// Create a text file filled entirely with 'a's
let allABytes2 = Array.init 1024 (fun _ -> byte 'a')

// FileToProcess should read "aaaaaaaaaaaaaaa..."
File.WriteAllBytes(@"C:\Scratch\FileToProcess2.txt", allABytes)

// Now update the file's contents asynchronously
let processData2 fileContents =
    fileContents
    |> Array.mapi (fun idx (b : byte) -> b + byte (idx % 26))
    
// After the async operation completes, 
// FileToProcess.txt.results will read "abcdefghijklmopqrstuv..."
asyncProcessFile @"C:\Scratch\FileToProcess2.txt" processData

// ----------------------------------------------------------------------------

#r @"D:\PowerBack Binaries\FSharp.PowerPack.dll"

open System.IO
open System.Net

open Microsoft.FSharp.Control.WebExtensions

let getHtml (url : string) =
    async {

        let req = WebRequest.Create(url)
        let! rsp = req.AsyncGetResponse()

        use stream = rsp.GetResponseStream()
        use reader = new StreamReader(stream)

        // synchronous read-to-end
        return! reader.AsyncReadToEnd()
    }
    
let html =
    getHtml "http://en.wikipedia.org/wiki/F_Sharp_programming_language"
    |> Async.RunSynchronously

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let webPages : string[] =
    [ "http://www.bing.com"; "http://www.google.com"; "http://www.yahoo.com" ]
    |> List.map getHtml
    |> Async.Parallel
    |> Async.RunSynchronously

// ----------------------------------------------------------------------------

// Test that async operations don't bring down the process
if Async.Start(async { raise <| new System.Exception("Bad things man") } ) <> () then failwith "ERROR"

// ----------------------------------------------------------------------------

let asyncTaskX = async { raise <| new System.Exception("asdf") }
//let asyncTaskX = async { return 1 }

asyncTaskX
|> Async.Catch
|> Async.RunSynchronously
|> function
    | Choice1Of2 result     -> printfn "The async operation completed with %A" result
    | Choice2Of2 (ex : exn) -> printfn "Exception thrown: %s" ex.Message

// ----------------------------------------------------------------------------

open System
open System.Threading

let cancelableTask =
    async {
        printfn "Waiting 10 seconds..."
        for i = 1 to 10 do
            printfn "%d..." i
            do! Async.Sleep(1000)
        printfn "Finished!"
    }
    
// Callback used when the operation is canceled
let cancelHandler (ex : OperationCanceledException) =
    printfn "The task has been canceled."
    
Async.TryCancelled(cancelableTask, cancelHandler)
|> Async.Start

// ...

Async.CancelDefaultToken()

// ----------------------------------------------------------------------------

open System.Threading

let computation        = Async.TryCancelled(cancelableTask, cancelHandler)
let cancellationSource = new CancellationTokenSource()

Async.Start(computation, cancellationSource.Token)

// ...

cancellationSource.Cancel()

// ----------------------------------------------------------------------------

let superAwesomeAsyncTask = async { return 1 }

Async.StartWithContinuations(
    superAwesomeAsyncTask,
    (fun result -> printfn "Task completed with result %A" result),
    (fun exn    -> printfn "Task threw an exception with Message: %s" exn.Message),
    (fun oce    -> printfn "Task was cancelled. Message: %s" oce.Message)
)

// ----------------------------------------------------------------------------    

open System.Threading

async {
    for i = 5 to 1 do
        printfn "%d..." i
        do! Async.Sleep(1000)
    printfn "Boom!" 
}
|> Async.Start

 // ----------------------------------------------------------------------------
 
#r "System.Windows.Forms.dll"
#r "System.Drawing.dll"

open System.Threading
open System.Windows.Forms

let form = new Form(TopMost = true)

let pb = new ProgressBar(Minimum = 0, Maximum = 15, Dock = DockStyle.Fill)
form.Controls.Add(pb)

form.Show()

// Launch a task asynchronously which will update the UI
async {
    for i = 0 to 15 do
        do! Async.Sleep(1000)
        pb.Value <- i
} |> Async.Start

// ----------------------------------------------------------------------------

#r "FSharp.PowerPack.dll"

open System.IO
open System.Net

open Microsoft.FSharp.Control.WebExtensions

let getHtml2 (url : string) =
    async {

        let req = WebRequest.Create(url)
        let! rsp = req.AsyncGetResponse()

        use stream = rsp.GetResponseStream()
        use reader = new StreamReader(stream)

        return reader.ReadToEnd()
    }
    
// ----------------------------------------------------------------------------

#r "System.Core.dll"

open System
open System.IO

type System.IO.Directory with
    /// Retrieve all files under a path asynchronously
    static member AsyncGetFiles(path : string, searchPattern : string) =
        let dele = new Func<string * string, string[]>(Directory.GetFiles)
        Async.FromBeginEnd(
            (path, searchPattern), 
            dele.BeginInvoke, 
            dele.EndInvoke)
        
type System.IO.File with
    /// Copy a file asynchronously
    static member AsyncCopy(source : string, dest : string) =
        let dele = new Func<string * string, unit>(File.Copy)
        Async.FromBeginEnd((source, dest), dele.BeginInvoke, dele.EndInvoke)


let asyncBackup path searchPattern destPath =
    async {
        let! files = Directory.AsyncGetFiles(path, searchPattern)
        
        for file in files do
            let filename = Path.GetFileName(file)
            do! File.AsyncCopy(file, Path.Combine(destPath, filename))
    }

// ----------------------------------------------------------------------------


#r "System.Core.dll"

open System
open System.Threading

/// Multiply two matricies using the PFX
let matrixMultiply (a : float[,]) (b : float[,]) =

    let aRow, aCol = Array2D.length1 a, Array2D.length2 a
    let bRow, bCol = Array2D.length1 b, Array2D.length2 b
    if aCol <> bRow then failwith "Array dimension mismatch."
    
    // Allocate space for the resulting matrix, c
    let c = Array2D.create aCol bRow 0.0
    let cRow, cCol = aCol, bRow
    
    // Compute each row of the resulting matrix
    let rowTask rowIdx =  
        for colIdx = 0 to cCol - 1 do
            for x = 0 to aRow - 1 do
                c.[colIdx, rowIdx] <- 
                    c.[colIdx, rowIdx] + a.[x, colIdx] * b.[rowIdx, x]
        ()
    
    let _ = Parallel.For(0, cRow, new Action<int>(rowTask))

    // Return the computed matrix
    c
    
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let ident = Array2D.zeroCreate 3 3
ident.[0, 0] <- 1.0
ident.[1, 1] <- 1.0
ident.[2, 2] <- 1.0

let mtx2 = Array2D.zeroCreate 2 3
mtx2.[0, 0] <- 4.0;     mtx2.[1, 0] <- 5.0
mtx2.[0, 1] <- 8.0;     mtx2.[1, 1] <- 1.0
mtx2.[0, 2] <- 3.0;     mtx2.[1, 2] <- 2.0

let result = matrixMultiply mtx2 ident

// ----------------------------------------------------------------------------

open System.IO

let getSecretData keyword =   

    let secretFiles = Directory.GetFiles(@"C:\Scratch\", "*.classified")

    Array.Parallel.iter File.Decrypt secretFiles

    let secretData = 
        Directory.GetFiles(@"C:\Scratch\", "*.classified")
        |> Array.Parallel.map (fun filePath -> File.ReadAllText(filePath))
        |> Array.Parallel.choose (fun contents -> 
            if contents.Contains(keyword) 
            then Some(contents)
            else None)

    Array.Parallel.iter File.Encrypt secretFiles

    secretData

// ----------------------------------------------------------------------------

let longTask1() = ""
let longTask2() = ""

// ------

open System.Threading


let mutable completed = false
let mutable result1 = null

ThreadPool.QueueUserWorkItem(fun _ ->
    result1   <- longTask1()
    completed <- true
) |> ignore
    
let result2 = longTask2()
   
// Wait until task1 complete
while not completed do
    Thread.Sleep(0)
    
result1, result2

// ----------------------------------------------------------------------------

open System.Threading.Tasks

let taskBody = new Func<string>(longTask1)
let task = Task.Factory.StartNew<string>(taskBody)

let result2 = longTask2()
let result1 = task.Result

// ----------------------------------------------------------------------------

#r "System.Core.dll"

open System
open System.IO
open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D
open System.Threading.Tasks

/// Resize images to a new width, height
let resizeImage (newWidth : int, newHeight : int) (filePath : string) =
    let originalImage = Bitmap.FromFile(filePath)
    
    let resizedImage = new Bitmap(newWidth, newHeight)
    use g = Graphics.FromImage(resizedImage)
    g.InterpolationMode <- InterpolationMode.HighQualityBicubic
    g.DrawImage(originalImage, 0, 0, newWidth, newHeight)
    
    let fileName = Path.GetFileNameWithoutExtension(filePath)
    let fileFolder = Path.GetDirectoryName(filePath)
    resizedImage.Save(
        Path.Combine(fileFolder, fileName + ".Resized.jpg"), 
        ImageFormat.Jpeg)

// Spawns a new PFX task to resize an image
let spawnTask filePath = 
    let taskBody = new Action(fun () -> resizeImage (640, 480) filePath)
    Task.Factory.StartNew(taskBody)
        
let imageFiles = Directory.GetFiles(@"C:\2009-03-16 iPhone\", "*.jpg")

// Spawning resize tasks
let resizeTasks = imageFiles |> Array.map spawnTask

// Wait for them all to complete
Task.WaitAll(resizeTasks)

// ----------------------------------------------------------------------------

open System
open System.Threading
open System.Threading.Tasks

let startLongRunningTask() =
    Task.Factory.StartNew(fun _ -> 
        let mutable i = 1
        let mutable loop = true
        
        while i <= 10 && loop do
            printfn "%d..." i
            i <- i + 1
            Thread.Sleep(1000)
            
            // Check if the task was cancelled
            if Task.Current.CancellationToken.IsCancellationRequested then
                printfn "Cancelled; stopping early."
                loop <- false
                
        printfn "Complete!"
    )

let t = startLongRunningTask()

t.Cancel()

// ----------------------------------------------------------------------------
                    
open System
open System.Threading.Tasks

type CaveNode =
    | ManEatingYeti of string
    | BottomlessPit
    | Treasure
    | DeadEnd
    | Fork of CaveNode * CaveNode
    
let rec findAllTreasure node =
    match node with
    
    // Finding treasure in magical caves ain't easy...
    | ManEatingYeti(n) -> failwithf "Eaten by a yeti named %s" n
    | BottomlessPit    -> failwith "Fell into a bottomless pit"
    
    // Leaf nodes
    | DeadEnd  -> 0
    | Treasure -> 1
    
    // Branches. Send half of your expidition left, the other half goes right...
    // ... hopefully they won't be eaten by a Yeti
    | Fork(left, right) ->
        let goLeft  = Task.Factory.StartNew<int>(fun () -> findAllTreasure left)
        let goRight = Task.Factory.StartNew<int>(fun () -> findAllTreasure right)
        goLeft.Result + goRight.Result

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

let colossalCave  = 
    Fork(
        Fork(
            DeadEnd, 
            Fork(Treasure, BottomlessPit)
        ),
        Fork(
            ManEatingYeti("Umaro"),
            Fork(
                ManEatingYeti("Choko"),
                Treasure
            )
        )
    )

try
    let treasureFindingTask = 
        Task.Factory.StartNew<int>(fun () -> findAllTreasure colossalCave)
    
    printfn "Found %d treasure!" treasureFindingTask.Result
    
with
| :? AggregateException as ae ->
    
    // Get all inner exceptions from the aggregate exception.
    // Alternately you can use the .Flatten() method.
    let rec decomposeAggregEx (ae : AggregateException) =
        seq {
            for innerEx in ae.InnerExceptions do
                match innerEx with
                | :? AggregateException as ae -> yield! decomposeAggregEx ae
                | _ -> yield innerEx 
        }
    
    printfn "AggregateException:"
    
    decomposeAggregEx ae
    |> Seq.iter (fun ex -> printfn "\tMessage: %s" ex.Message)

// ----------------------------------------------------------------------------

// Concurrent Queue
open System.Collections.Concurrent

let cq = new ConcurrentQueue<int>()
[1 .. 10] |> List.iter cq.Enqueue

let success, firstItem = cq.TryDequeue()
if success then
    printfn "%d was successfully dequeued" firstItem
else
    printfn "Data race, try again later"

// ----------------------------------------------------------------------------

open System
open System.IO
open System.Collections.Concurrent

let d = new ConcurrentDictionary<_,_>()

// Queue of files to process
let filesToProcess =
    Directory.GetFiles(@"D:\Book\", "*.txt")
    |> (fun files -> new ConcurrentQueue<_>(files))
    
// Dictionary to store the occurances of particular words
let wordUsage = new ConcurrentDictionary<string, int>()

let processFile filePath =
    let text = File.ReadAllText(filePath)
    let words =
        text.Split([| ' '; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun word -> word.Trim())
        
    // Add the word to our lookup table. Inserts value '1', or if the key
    // is already present updates it to be 'count + 1'.
    Array.Parallel.iter(fun word  ->
        wordUsage.AddOrUpdate(word, 1, fun count -> count + 1)
    ) words

// Begin updating the wordUsage dictionary in the background
let fillDictionary() =
    let mutable continueWorking = true

    while continueWorking do

        let dequeueSuccessful, filePath = filesToProcess.TryDequeue()
        
        if not dequeueSuccessful then
            // If the queue is empty, then we are done working
            if filesToProcess.IsEmpty then
                continueWorking <- false
            // ... otherwise, two tasks tried to dequeue
            // at the same time. Try again.
            else
                continueWorking <- true
                
        // Process the file
        processFile filePath

// Start three tasks to count word usage
fillDictionary()
fillDictionary()
fillDictionary()

// ----------------------------------------------------------------------------

open System
open System.Threading
open System.Collections.Concurrent

let getFactors x =
    [
        for i = 1 to x do
            if x % i = 0 then
                yield i
    ]
    
let getFactorDict max =
    let factorDict = new ConcurrentDictionary<int, int list>()
    
    let updateDict i =
        if factorDict.TryAdd(i, getFactors i) = false then
            printfn "Factors for %d already in dictionary..." i
    
    let _ = Parallel.For(0, max, (fun i -> updateDict i))
    
    factorDict

// ----------------------------------------------------------------------------

#r "System.Core.dll"

open System
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Collections.Generic

/// Check if a number is prime
let isPrime x =
    let rec primeCheck count =
        // If the counter has reaced x, then we know x is prime
        if count = x       then true
        // If x is divisible by the counter, we know x isn't prime
        elif x % count = 0 then false
        else primeCheck (count + 1)
    // Special case 1 as prime
    if x = 1 
    then true
    else primeCheck 2
    
let computePrimes tasksToSpawn maxValue =
    let range = maxValue / tasksToSpawn
    let primes = new ConcurrentBag<int>()

    // Spawn several tasks at once, adding any primes they find
    // to the ConcurrentBag
    let tasks =
        [|  
        for i in 0 .. tasksToSpawn - 1 do
            yield Task.Factory.StartNew(
                Action(fun () -> 
                    for x = i * range to (i + 1) * range - 1 do
                        if isPrime x then primes.Add(x)
                )
            )
        |]
    Task.WaitAll(tasks)
    
    new HashSet<_>(primes :> seq<int>)
