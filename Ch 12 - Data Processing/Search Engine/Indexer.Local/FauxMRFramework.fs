// Copyright 2012 Chris Smith. All Rights Reserved.

module Indexer.Local.FauxMapReduceFramework

open System
open System.Collections.Generic
open System.IO
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

open Indexer.Mappers
open Indexer.Reducers

/// Runs a map reduce job on the local machine in sequence and in memory.
let runMapReduceJob (mapper : Mapper<'k, 'v>) (reducer : Reducer<'k, 'v, 'output>) (input : seq<string>) =
    let inputLength = Seq.length input |> double
    printfn "Mapping over %d inputs" (int inputLength)

    let logMapEvery = 100
    let logReduceEvery = 2000

    // Map
    let mapResults = 
        input
        |> Seq.mapi (fun idx lineToProcess -> if idx % logMapEvery = 0 then
                                                  let percentComp = (double idx) / inputLength
                                                  printfn "\tMapping %s [%f]" lineToProcess percentComp
                                              let results = mapper.Map(lineToProcess)
                                              results)
        |> Seq.concat
        |> Seq.cache
    
    // Shuffle/Sort
    let shuffledResults =
        mapResults
        |> Seq.groupBy fst
        |> Seq.map(fun (key, values) -> let values' = Seq.map snd values
                                        key, values')

    // Reduce
    let itemsToReduce = double <| Seq.length shuffledResults
    printfn "Reducing %d outputs" (int itemsToReduce)

    let results =
        shuffledResults    
        |> Seq.mapi(fun idx (key, values) -> if idx % logReduceEvery = 0 then
                                                 let percentComp = (double idx) / itemsToReduce
                                                 printfn "\tReducing %O [%f]" key percentComp
                                             let results = reducer.Reduce(key, values)
                                             results)
        |> Seq.concat
        |> Seq.cache

    // Force the expansion of results. If we evaluate things lazily, we cannot GC things like
    // the individual mapper and reducer. Also, worst case scenario, clients iterate through
    // the results more than once and we run the MR job AGAIN!
    printfn "Finished running map reduce."
    new List<_>(results)

/// Runs a map reduce job on the local machine in sequence.
let serializeObject thing filePath =
    let formatter = new BinaryFormatter()
    use stream = new FileStream(filePath, FileMode.Create,
                                FileAccess.Write, FileShare.None)
    formatter.Serialize(stream, thing)
    stream.Close()

let serializeObjectBase64 thing filePath =
    use memoryStream = new MemoryStream()
    let binaryFormatter = new BinaryFormatter()
    binaryFormatter.Serialize(memoryStream, thing)
    let stringRepresentation = System.Convert.ToBase64String(memoryStream.ToArray())
    stringRepresentation

let deserializeObject filePath =
    let formatter = new BinaryFormatter();
    use stream = new FileStream(filePath, FileMode.Open, 
                                FileAccess.Read, FileShare.Read)
    let thing = formatter.Deserialize(stream)
    stream.Close()

    thing

let deserializeObjectBase64 text =
    let memorydata = Convert.FromBase64String(text)
    use memoryStream = new MemoryStream(memorydata);
    let bf = new BinaryFormatter()
    bf.Deserialize(memoryStream)
                    
/// Runs a map reduce job in memory, saving intermediate results to disk. This can handle slightly
/// larger datasets before thrashing, but not by much.
let runMapReduceJobToDisk (mapper : Mapper<'k, 'v>) (reducer : Reducer<'k, 'v, 'output>) (input : seq<string>) =
    let inputLength = Seq.length input |> double
    printfn "Mapping over %d inputs" (int inputLength)

    let logMapEvery = 100
    let logReduceEvery = 2000

    let outputFolder = Path.Combine(Path.GetTempPath(), DateTime.Now.Ticks.ToString())
    Directory.CreateDirectory(outputFolder) |> ignore
    printfn "Writing results to '%s'" outputFolder

    let mapperOutputFile outputFolder i max = 
        let fileName = String.Format("mapper--{0:D6}-of-{1:D6}.bin", i, max)
        Path.Combine(outputFolder, fileName)

    let reducerInputFile outputFolder (key : obj) = 
        let fileName = String.Format("reducer--{0:X8}.bin", key.GetHashCode())
        Path.Combine(outputFolder, fileName)

    // Map
    let inputList = new List<_>(input)
    printfn "Mapping %d inputs" inputList.Count
    for i = 0 to inputList.Count - 1 do
        let lineToProcess = inputList.[i]
        if i % logMapEvery = 0 then
            let percentComp = (double i) / inputLength
            printfn "\tMapping %s [%f]" lineToProcess percentComp
        let results = mapper.Map(lineToProcess)
        
        // Save mapper results to disk.
        let listResults = new List<_>(results)
        let outputFile = mapperOutputFile outputFolder i inputList.Count
        serializeObject listResults outputFile
    
    // Shuffle/Sort
    printfn "Shuffling %d mapper outputs" inputList.Count
    for i = 0 to inputList.Count - 1 do
        let outputFile = mapperOutputFile outputFolder i inputList.Count
        let rawResultsObj = deserializeObject outputFile
        let results = rawResultsObj :?> List<'k * 'v>

        // Nuke the mapper input go give an idea of how far we are along.
        File.Delete(outputFile)

        // Mapping of keys to output stream. This way we don't need to open/close
        // several thousand files several thousands of times each.
        // Hard drive, you are welcome.
        let shuffledOutputFileLookup = new Dictionary<'k,StreamWriter>()

        for j = 0 to results.Count - 1 do
            let key = fst <| results.[j]
            let value = snd <| results.[j]

            let serializedValue = serializeObjectBase64 value

            let reducerFilePath = reducerInputFile outputFolder key

            // Write file header and add it to our lookup. Otherwise append.
            if not <| shuffledOutputFileLookup.ContainsKey(key) then
                let serializedKey = serializeObjectBase64 key
                let reducerInputFile = File.AppendText(reducerFilePath)
                reducerInputFile.WriteLine(serializedKey)
                shuffledOutputFileLookup.Add(key, reducerInputFile)
            else
                shuffledOutputFileLookup.[key].WriteLine(serializedValue)

        // Close all output file streams.
        for fileStream in shuffledOutputFileLookup.Values do
            fileStream.Close()

    // Reduce
    let output = new List<'output>(inputList.Count * 4)

    let filesToReduce = Directory.GetFiles(outputFolder, "reducer--*")
    for i = 0 to filesToReduce.Length - 1 do
        let allFileLines = 
            filesToReduce.[i]
            |> File.ReadAllLines

        // Nuke to indicate progress.
        File.Delete(filesToReduce.[i])

        let key = (deserializeObjectBase64 allFileLines.[0]) :?> 'k

        let reducerInputs : seq<'v> =
            allFileLines
            |> Seq.skip 1  // Skip header or serialized key
            |> Seq.map deserializeObjectBase64
            |> Seq.map(fun o -> o :?> 'v)
        
        if i % logReduceEvery = 0 then
            let percentComp = (double i) / (float filesToReduce.Length)
            printfn "\tReducing %O [%f]" key percentComp
        
        let results = reducer.Reduce(key, reducerInputs)
        output.AddRange(results)

    // Force the expansion of results. If we evaluate things lazily, we cannot GC things like
    // the individual mapper and reducer. Also, worst case scenario, clients iterate through
    // the results more than once and we run the MR job AGAIN!
    printfn "Finished running map reduce."
    output