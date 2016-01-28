open System
// This is example code for uploading files to linux/Apache/CGI
// using an F# console app. Needs a ton of work if you're going
// to use it in anything like production: esp security and testing

// Yep. Gotta use manual debugging. Set to true to write ton of files
let DEBUG=true
let DEBUG_WRITE_DIRECTORY="../scripts/" // set this somewhere you can write
let writeDebugFile(filename,contents) =
    if DEBUG then System.IO.File.WriteAllText(DEBUG_WRITE_DIRECTORY + filename, contents) else ()
let writeDebugTextLine(tw:System.IO.TextWriter) (stuffToWrite:string) =
    if DEBUG then tw.WriteLine stuffToWrite
// Note that you can also write to the Apache error log for your website like so -> Console.Error.WriteLine "Boo!"

// overload .NET type system to do stuff I do commonly
type 'T ``[]`` with   //
    member this.Slice(indices:int array) =
        [| for i in indices -> this.[i] |]
    member this.SplitAtIndex(idx:int) =
        if this.Length>0 && idx<this.Length then (this.Slice([|0..idx|]),this.Slice([|idx..this.Length-1|])) else (this,[||])
    member this.RemoveXItemsFromTop(numItems:int) =
        if numItems+1 < this.Length then snd (this.SplitAtIndex numItems) else this
    member this.RemoveXItemsFromBottom(numItems:int) =
        if numItems+1 < this.Length then fst (this.SplitAtIndex (this.Length-numItems)) else this
    member this.RemoveXItemsFromTopYItemsFromBottom (x:int) (y:int) =
        (this.RemoveXItemsFromTop x).RemoveXItemsFromBottom y
type System.Collections.IDictionary with
    member this.toSeq =
        seq {for i = 0 to this.Count - 1 do yield this.[i]}
    member this.toArray =
        [|for i = 0 to this.Count - 1 do yield this.[i] |]

// helper functions for this library that I prob won't use elsewhere
let concatStringArrayWithSpacer (spacer:string) (stringArr:string[])  =
    let sBuff = new Text.StringBuilder(65535)
    stringArr |> Array.iteri(fun i x->
        sBuff.Append(x) |> ignore
        sBuff.Append(spacer) |> ignore
        )
    if stringArr.Length>0 then sBuff.ToString().TrimEnd(spacer.ToCharArray()) else sBuff.ToString()
let splitStringArrayAndConcatTop (spacer:string) (idx:int) (stringArr:string[]) =
    let splitArray = stringArr.SplitAtIndex idx
    (fst splitArray |> concatStringArrayWithSpacer spacer, snd splitArray)
let splitStringArrayAndConcatBottom (spacer:string) (idx:int) (stringArr:string[]) =
    let splitArray = stringArr.SplitAtIndex idx
    (fst splitArray, snd splitArray |> concatStringArrayWithSpacer spacer)
let splitStringArrayAtIndexThenConcatTopAndBottomWithSpacer (spacer:string) (idx:int) (stringArr:string[]) =
    let splitArray = stringArr.SplitAtIndex idx
    ((fst splitArray |> concatStringArrayWithSpacer spacer), snd splitArray |> concatStringArrayWithSpacer spacer)

    // Here's where we get the thing uploaded and make some sense out of it
let processUpload (uploadContent:string) =
    writeDebugFile("uploadContent.txt", uploadContent)
    // Need to figure out where line breaks are, and they could be different between client and server
    let firstLineFeed = if uploadContent.Contains("\n") then uploadContent.IndexOf("\n") else 0
    let clientSideLineFeed = 
        if firstLineFeed >0
            then if uploadContent.Chars(firstLineFeed-1)='\r' then "\r\n" else "\n"
            else "\n"
    let clientSideEmptyLine = clientSideLineFeed + clientSideLineFeed
    let contentWithoutBoundaryFrame = 
        let allPossibleClientLineFeeds = uploadContent.Split([|clientSideLineFeed|], StringSplitOptions.None)
        let removeTopBoundary = (allPossibleClientLineFeeds.RemoveXItemsFromTop 1)
        let removeBottomBoundary = removeTopBoundary.RemoveXItemsFromBottom 3
        removeBottomBoundary |> concatStringArrayWithSpacer clientSideLineFeed
    let headerContentSplit = 
        writeDebugFile("contentWithoutBoundaryFrame.txt", contentWithoutBoundaryFrame)
        let contentSplitByPossibleClientSentEmptyLines = contentWithoutBoundaryFrame.Split([|clientSideEmptyLine|], StringSplitOptions.None)
        let headerString =  if contentSplitByPossibleClientSentEmptyLines.Length>0 
                            then contentSplitByPossibleClientSentEmptyLines.[0] else ""
        let contentString = if contentSplitByPossibleClientSentEmptyLines.Length>0 
                            then contentSplitByPossibleClientSentEmptyLines.Slice [|1..contentSplitByPossibleClientSentEmptyLines.Length-1|] 
                                |> concatStringArrayWithSpacer clientSideLineFeed
                            else ""
        (headerString, contentString)
    let headerArrayAndContent = 
        writeDebugFile("fst_headerContentSplit.txt", (fst headerContentSplit))
        let headerContent = (fst headerContentSplit)
        let headerArraySplit = headerContent.Split([|clientSideEmptyLine|], StringSplitOptions.None)
        let headerDictionary = new System.Collections.Generic.Dictionary<string,string>()
        let headers =
            headerArraySplit |> Array.iter(fun x->
                let headerKeyValueSplit = x.Split([|':'|])
                if headerKeyValueSplit.Length>0
                    then
                        let key = headerKeyValueSplit.[0]
                        let value = (headerKeyValueSplit.Slice [|1..headerKeyValueSplit.Length-1|]) |> concatStringArrayWithSpacer clientSideLineFeed
                        headerDictionary.Add(key,value)
                    else
                        ()
                )
        (headerDictionary, (snd headerContentSplit))
    headerArrayAndContent

let httpRequestPost args (environmentVariables:System.Collections.Generic.IDictionary<string, string>)  = 
    let stdinStream = System.Console.In
    let uploadFileData (inStream:System.IO.TextReader) = async {
        let ct = inStream.ReadToEnd()
        return ct
    }
    let uploadedFileStringContents = uploadFileData stdinStream |> Async.RunSynchronously
    // Remember if you write something, shouldn't be able to write to cgi-bin, it's execute only
    let processedUpload = processUpload uploadedFileStringContents
    writeDebugFile("processedUpload.txt", (snd processedUpload))
    // you also need to write something back out to the web client that is uploading this file to let them know how it went
    System.Console.WriteLine("Content-Type: text/html")
    System.Console.WriteLine("") // have to have an empty line here to separate header from returning content
    System.Console.WriteLine("<html>\r\n<head>\r\n<title>Thanks!</title>\r\n</head>\r\n<body>\r\n")
    System.Console.WriteLine("<h1>done and done</h1>\r\n")
    writeDebugTextLine System.Console.Out ((fst processedUpload) |> Seq.mapi(fun i x-> i.ToString() + ". " + x.Key + " - " + x.Value) |> Seq.toArray |> concatStringArrayWithSpacer ("<br/>" + "\r\n"))
    writeDebugTextLine System.Console.Out (environmentVariables |> Seq.mapi(fun i x->i.ToString() + ". " + x.Key + " - " + x.Value) |> Seq.toArray |> concatStringArrayWithSpacer ("<br />" + "\r\n"))
    System.Console.WriteLine("</body>\r\n</html>\r\n")
    0

// If you want to blow this out into a full-fledged program, you'd fill out the switchboard, below
// This code only does single file upload and form submission via POST
[<EntryPoint>]
let main argv = 
    let environmentVariables = System.Environment.GetEnvironmentVariables() |> Seq.cast<System.Collections.DictionaryEntry> |> Seq.map(fun d->d.Key :?> string, d.Value :?> string) |> dict
    match environmentVariables.Item("REQUEST_METHOD") with
        | "GET" -> 0
        | "HEAD" -> 0
        | "DELETE" -> 0
        | "TRACE" -> 0
        | "OPTIONS" -> 0
        | "CONNECT" -> 0
        | "PATCH" -> 0
        | "PUT" -> 0
        | "POST" -> httpRequestPost argv environmentVariables
        |_ -> 0

// What to do if it doesn't work:
// When you build it, move all the compiled stuff to your linux dir, then run mkbundle with deps and static option
// Be sure your form is pointing to the website/program that you are working on (Yes, this happens)
// Be sure your permissions are right. This code has to run. Files being written out have to go into dirs with Write permission
// Read your apache error log. If the code is running, it will barf into that (usually in WEBISTE/logs/error.log)
// Be sure your form uses name(s) as attributes on all your form fields. Id(s) won't work.
// Set the DEBUG to true at top of this file. It'll write out stuff as the code progresses. Look at this stuff.
// With DEBUG true, I've got it dumping the headers to the result page. See if there's any CONTENT_LENGTH.
// Sprinkle some Console.Error.WriteLine(s) around, then run, then check logs again. Fun times.
