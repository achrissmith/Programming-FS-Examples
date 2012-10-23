// Script file for downloading English texts from Project Gutenberg's website.
//
// Be kind to Project Gutenberg, keep the delays between requests as-is.
// http://www.gutenberg.org/wiki/Gutenberg:Information_About_Robot_Access_to_our_Pages

open System.IO
open System.IO.Compression
open System.Net
open System.Text.RegularExpressions
open System.Threading

// Get files via:
// wget -w 2 -m -r -k "http://www.gutenberg.org/robot/harvest?filetypes[]=txt&langs[]=en"
// dir -b > files.txt

let pathToMasterFile  = @"C:\Users\chrsmith\Desktop\Gutenberg\files.txt"
let outputPath = @"C:\Users\chrsmith\Desktop\Gutenberg\downloaded\"
let rootFolder = @"C:\Users\chrsmith\Desktop\Gutenberg\www.gutenberg.org\robot\"

let getAnchorLinks filePath =
    let filePath = Path.Combine(rootFolder, filePath)
    let fileContents = File.ReadAllText(filePath)
    let hrefAddressRegEx = new Regex("href=\"([^\"]+)\"")
    let matches = hrefAddressRegEx.Matches(fileContents)

    matches
    |> Seq.cast
    |> Seq.map (fun (m : Match) -> m.Groups.[1].Value)

let downloadUrl url =
    let webClient = new WebClient()
    let downloadedFilePath = Path.Combine(outputPath, Path.GetFileName(url))
    if not <| File.Exists(downloadedFilePath) then
        webClient.DownloadFile(url, downloadedFilePath)
        Thread.Sleep(2000)
    else
        printfn "\tSkipping..."

let downloadAllFiles() =
    File.ReadAllLines(pathToMasterFile)
    |> Seq.map getAnchorLinks
    |> Seq.concat
    |> Seq.filter(fun (url : string) -> url.ToLowerInvariant().EndsWith(".zip"))
    |> Seq.map(fun url -> Thread.Sleep(2000); url)
    |> Seq.map(fun url -> printfn "downloading %s" url; url)
    |> Seq.iter downloadUrl

let decompress filePath =
    let fileInfo = new FileInfo(filePath)
    use inFile = fileInfo.OpenRead()

    let curFile = fileInfo.FullName
    let origName = curFile.Remove(curFile.Length - fileInfo.Extension.Length)

    use outFile = File.Create(origName)
    use decompressStream = new GZipStream(inFile, CompressionMode.Decompress)
    decompressStream.CopyTo(outFile)

// The following is used for extracting title information from
// the PG catalog.rdf

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

// Use a regular expression to capture three groups
let (|RegexMatch2|_|) (pattern : string) (input : string) =
    let result = Regex.Match(input, pattern)
    if result.Success then
        match (List.tail [ for g in result.Groups -> g.Value ]) with
        | fst :: snd :: []
             -> Some (fst, snd)
        | [] -> failwith <| "Match succeeded, but no groups found.\n" +
                            "Use '(.*)' to capture groups"
        | _  -> failwith "Match succeeded, but did not find exactly three groups."
    else
        None

let tryParseLine input =
    match input with
    | RegexMatch2 "(.+) (\d+)" (title, etextNumber) -> Some(title, etextNumber)
    | _ -> None

let CatalogFile = "C:\ProjectGutenbergTextsFull\GUTINDEX.ALL"
let catalogFileLines = File.ReadAllLines(CatalogFile)

let generateCatalog() =
    let catalogFiles = new Dictionary<string, string>()
    let addTitle title textNumber =
        if catalogFiles.ContainsKey(textNumber) then
            catalogFiles.[textNumber] <- title
        else
            catalogFiles.Add(textNumber, title)

    catalogFileLines
    |> Seq.map tryParseLine
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.iter(fun (title, textNumber) -> addTitle title textNumber)

    printfn "Catalog files: %d" catalogFiles.Count

    catalogFiles
