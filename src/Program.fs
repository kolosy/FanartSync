// Learn more about F# at http://fsharp.net

open System
open System.Net
open System.Xml
open System.IO
open ICSharpCode.SharpZipLib.Zip

let SourceFile = "--source-file"
let TargetDir = "--target-dir"
let FolderByShow = "--folder-by-show"
let ApiKey = "--api-key"
let Overwrite = "--overwrite-existing"

let makeRequest host uri extra =
    (((new UriBuilder("http", host, 80, uri, extra)).Uri
        |> WebRequest.Create) :?> HttpWebRequest).GetResponse()

let getMirrors apiKey = 
    use req = makeRequest "www.thetvdb.com" ("/api/" + apiKey + "/mirrors.xml") ""
    let xml = (new XmlDocument())
    xml.Load(req.GetResponseStream())
    let all = 
        xml.SelectNodes "/Mirrors/Mirror"
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> node.SelectSingleNode("mirrorpath").InnerText.Replace("http://", ""), (node.SelectSingleNode("typemask").InnerText |> Convert.ToInt32))
        |> List.ofSeq

    let xml = List.filter (fun (host, mask) -> mask &&& 1 = 1) all |> List.unzip |> fst
    let banner = List.filter (fun (host, mask) -> mask &&& 2 = 2) all |> List.unzip |> fst
    let zip = List.filter (fun (host, mask) -> mask &&& 4 = 4) all |> List.unzip |> fst

    xml, banner, zip

let rec position (callbacks: (string * (ZipInputStream -> XmlDocument)) list) (zipStream: ZipInputStream) = 
    let entry = zipStream.GetNextEntry()
    if not (entry = null) then
        match List.tryPick (fun (name, cb) -> if entry.Name = name then Some (name, cb zipStream) else None) callbacks with
        | Some v -> v :: position callbacks zipStream
        | None -> position callbacks zipStream
    else
        []

let getShowBanners zipMirrors apiKey showId =
    use req = makeRequest (List.head zipMirrors) (sprintf "/api/%s/series/%s/all/en.zip" apiKey showId) ""
    let decoder (zipStream: ZipInputStream) =
        let doc = new XmlDocument()
        doc.Load(zipStream)
        doc

    let docs = 
        position [ "en.xml", decoder; "banners.xml", decoder; ] (new ZipInputStream(req.GetResponseStream()))
        |> Map.ofList

    let invalidChars = [ Path.GetInvalidFileNameChars(); Path.GetInvalidFileNameChars() ] |> Array.concat
    ((docs.["en.xml"]).SelectSingleNode "/Data/Series/SeriesName").InnerText |> String.map (fun c -> if Array.exists ((=) c) invalidChars then '_' else c),
        (docs.["banners.xml"]).SelectNodes "/Banners/Banner[BannerType=\"fanart\"]"
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> (node.SelectSingleNode "BannerPath").InnerText)

let saveServerTime dir =
    use req = makeRequest "www.thetvdb.com" "/api/Updates.php" "?type=none"
    let xmlDoc = new XmlDocument()
    xmlDoc.Load(new StreamReader(req.GetResponseStream()))
    let fName = Path.Combine(dir, ".lastupdate")
    if File.Exists fName then File.Delete fName else()
    use writer = File.CreateText fName
    writer.Write(xmlDoc.InnerText)

let getServerTime dir = 
    let fName = Path.Combine(dir, ".lastupdate")
    if not (File.Exists fName) then None
    else Some (File.ReadAllText fName)    

let saveLocal bannerMirrors showId dir overwrite path =
    try
        use req = makeRequest (List.head bannerMirrors) (sprintf "/banners/%s" path) ""
        use stream = req.GetResponseStream()
        let fileName = Path.Combine(dir, path.[path.LastIndexOf('/')+1..])

        if (not (Directory.Exists dir)) then Directory.CreateDirectory dir |> ignore else ()
        if (not (File.Exists fileName) || overwrite) then
            if File.Exists fileName then File.Delete fileName else ()
            use fos = File.Create fileName
            let buffer = Array.create 2048 0uy
            let mutable r = stream.Read(buffer, 0, buffer.Length)
            while not (r = 0) do
                fos.Write(buffer, 0, r)
                r <- stream.Read(buffer, 0, buffer.Length)
        else
            printfn "%s already exists. skipping" path
    with
    | :? WebException as e -> printf "%A when downloading %s %s. skipping." e.Message showId path

let filterByTime dir lastTime (series: string seq) = 
    let filteredSeries = 
        series
        |> Seq.filter (fun show -> not (show.StartsWith("#")))
        |> Seq.map (fun show -> show.Trim().[0..(match show.IndexOf('#') with | -1 -> show.Length-1 | _ as v -> v-1)])

    match lastTime with 
    | None -> filteredSeries
    | Some time ->
        use req = makeRequest "www.thetvdb.com" "api/Updates.php" ("?type=all&time=" + time)
        let xml = (new XmlDocument())
        xml.Load(req.GetResponseStream())

        xml.SelectNodes "/Items/Series"
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> node.InnerText)
        |> Set.ofSeq
        |> Set.intersect (Set.ofSeq filteredSeries)
        |> Set.toSeq
    |> List.ofSeq

if Environment.GetCommandLineArgs().Length = 1 then
    failwith "No arguments supplied"
elif Environment.GetCommandLineArgs().Length % 2 = 0 then
    failwith "Malformed argument string"
else
    let args = Environment.GetCommandLineArgs().[1..]
    
    let argMap = 
        [for i in 0 .. 2 .. ((Array.length args)-1) do 
            yield (args.[i], args.[i+1])]
        |> Map.ofList

    let lastTime = getServerTime argMap.[TargetDir]
    saveServerTime (argMap.[TargetDir])

    let xml, banner, zip = getMirrors (argMap.[ApiKey])
    use fStream = new StreamReader(File.OpenRead (Path.Combine(argMap.[TargetDir], argMap.[SourceFile])))
    let shows = 
        seq { while not fStream.EndOfStream do yield fStream.ReadLine() }
        |> List.ofSeq
        |> filterByTime argMap.[TargetDir] lastTime

    if lastTime.IsSome then
        if List.length shows = 0 then
            printf "no changes since last load. if you want to re-run the full load, delete the .lastupdate file.\r\n"
        else
            printf "The following shows will be updated: \r\n"
            List.iter (printf "%s\r\n") shows
    else
        printf "new load. all shows will be updated.\r\n"

    shows
    |> Seq.map (getShowBanners zip (argMap.[ApiKey]))
    |> Seq.iter (fun (show, path) -> 
                    printf "loading %s\r\n" show
                    path |>
                    Seq.iter (
                        saveLocal 
                            banner 
                            show 
                            (if Map.exists (fun k (t: string) -> k = FolderByShow && t.ToLower() = "true") argMap then Path.Combine(argMap.[TargetDir], show) else argMap.[TargetDir]) 
                            (Map.exists (fun k (t: string) -> k = Overwrite && t.ToLower() = "true") argMap)) 
                    ) 

    printf "done. press enter to quit."
    Console.ReadLine() |> ignore