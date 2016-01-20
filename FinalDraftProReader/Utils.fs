module Utils
    open Types
    open System

    ///
    /// Common Utilities for this family of programs
    ///

    let commandLinePrintWhileEnter (opts:ConfigBase) fnPrintMe =
                // Entering program command line report
            match opts.verbose.parameterValue with
                | Verbosity.Silent ->
                    ()
                | Verbosity.BatchMinimum ->
                    printfn "%s" opts.programName
                | Verbosity.Minimum ->
                    printfn "Begin %s. %s" opts.programName opts.programTagLine
                | Verbosity.BatchNormal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                | Verbosity.Normal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    printfn "Verbosity: Normal" 
                | Verbosity.BatchVerbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                | Verbosity.Verbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                |_ ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()

    let commandLinePrintWhileExit (baseOptions:ConfigBase) =
            // Exiting program command line report
        match baseOptions.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                printfn "End %s" baseOptions.programName
            | Verbosity.BatchNormal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Normal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.BatchVerbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Verbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            |_ ->
                ()
    let createNewConfigEntry commandlineSymbol commandlineParameterName parameterHelpText initialValue =
        {
            commandLineParameterSymbol=commandlineSymbol
            commandLineParameterName=commandlineParameterName
            parameterHelpText=parameterHelpText
            parameterValue=initialValue
        }


    let loadConfigFromCommandLine (programName:string) (programTagLine:string) (programHelpText:string []) (defaultInputFileName:string) (defaultOutputFileName:string) (args:string []) =
        let defaultVerbosityConfigEntry  =
            {
                commandLineParameterSymbol="V"
                commandLineParameterName="Verbosity"
                parameterHelpText=[|"/V:[0-9]           -> Amount of trace info to report. 0=none, 5=normal, 9=max."|]           
                parameterValue=Verbosity.Normal
            }
        let defaultInputFileNameConfigEntry = createNewConfigEntry "I" "Input File" [|"/I:<filename> -> Filename for input."; ("Default is " + defaultInputFileName)|] defaultInputFileName
        let defaultOutputFileNameConfigEntry = createNewConfigEntry "O" "Output File" [|"/O:<filename> -> Filename output"; ("Default " + defaultOutputFileName)|] defaultOutputFileName
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosityConfigEntry, args)
        let newInterimProgress = {items=new System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()}
        let newInputFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultInputFileNameConfigEntry, args)
        let newInputFileExists = System.IO.File.Exists(defaultInputFileNameConfigEntry.parameterValue)
        let newOutputFileName = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFileNameConfigEntry, args)
        let newOutputFileExists = System.IO.File.Exists(defaultOutputFileNameConfigEntry.parameterValue)
        let newConfigBase =
            {
                programName = programName
                programTagLine = programTagLine
                programHelpText = programHelpText
                verbose = newVerbosity
                interimProgress=newInterimProgress
                inputFileName=newInputFileName
                outputFileName=newOutputFileName
                inputFileExists=newInputFileExists
                outputFileExists=newOutputFileExists
            }
        let newDummyVariable = ""
        {
            configBase=newConfigBase
            dummyVariable=newDummyVariable
        }

    let isLinuxFileSystem =
        let os = Environment.OSVersion
        let platformId = os.Platform
        match platformId with
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE | PlatformID.Xbox -> false
            | PlatformID.MacOSX | PlatformID.Unix -> true
            | _ ->false
    let copyToDestinationDirectory (localFileName:string) (copyTo:string) =
        if System.IO.File.Exists(localFileName) = false
            then
                ()
            else
                if not isLinuxFileSystem
                    then
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"cmd.exe"
                        systemProc.StartInfo.Arguments<-("/C copy " + localFileName + " " + copyTo)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()                
                    else
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"/bin/cp"
                        systemProc.StartInfo.Arguments<-(" " + localFileName + " " + copyTo)
                        //System.Console.WriteLine (systemProc.StartInfo.FileName + systemProc.StartInfo.Arguments)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()

    let runProgram fnProg argv programName programTagLine programHelpText defaultInputFileName defaultOutputFileName =
        let opts = loadConfigFromCommandLine programName programTagLine programHelpText defaultInputFileName defaultOutputFileName argv
        try                
            commandLinePrintWhileEnter opts.configBase (opts.printThis)
            fnProg opts
            commandLinePrintWhileExit opts.configBase
            0
        with
            | :? UserNeedsHelp as hex ->
                printfn "%s: %s" opts.configBase.programName hex.Data0
                opts.printCommandLineParameterHelp()
                0
            | :? System.Exception as ex ->
                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                System.Console.WriteLine (ex.StackTrace)
                if ex.InnerException = null
                    then
                        opts.printThis()
                        0
                    else
                        System.Console.WriteLine("---   Inner Exception   ---")
                        System.Console.WriteLine (ex.InnerException.Message)
                        System.Console.WriteLine (ex.InnerException.StackTrace)
                        0

    ///
    /// Utility functions for this specific program
    ///

    let getXMLNodeAttributeValueOrEmptyString (node:System.Xml.XmlNode) (attributeName:string) =
        let attribute = node.Attributes.[attributeName]
        if attribute = null then "" else attribute.Value
    let nodeHasThisAttributeValue (nd:System.Xml.XmlNode) (attributeName:string) (attributeValue:string) =
        (getXMLNodeAttributeValueOrEmptyString nd attributeName)=attributeValue
    let getAttributeValueArrayForMatchingXMLNodes (nd:System.Xml.XmlNode) (attributeName:string) (xpathSearch:string) =
        nd.SelectNodes(xpathSearch) |> Seq.cast<System.Xml.XmlNode> |> Seq.map(fun x->getXMLNodeAttributeValueOrEmptyString x attributeName) |> Seq.filter(fun x->x<>"") |> Seq.toArray
    let getInnerTextArrayForMatchingXMLNodes (nd:System.Xml.XmlNode) (xpathSearch:string)  = 
        nd.SelectNodes(xpathSearch) |> Seq.cast<System.Xml.XmlNode>|> Seq.map(fun x->x.InnerText) |> Seq.toArray
    let putSeparatorIntoStringListWhileConcatentating (stringConcatentaor:string) (stringList: string []) =
        let ret = (stringList |> Array.map(fun x->x + stringConcatentaor) |> Array.fold(fun acc x->x + acc) "")
        if ret.Length >= stringConcatentaor.Length then ret.Substring(0, ret.Length-stringConcatentaor.Length) else ""
    let putInQuotesAndSeparateByCommas (stringList:string []) = 
        stringList |> Array.map(fun x->x.WrapInQuotes + ",") |> Array.fold(fun acc x->acc + x) ""
    let scenesThatArentPhysicalLocations = ["TITLE CARD";"SECTION BREAK"]
    let scriptNodeTypesThatShouldAlwaysBeAllCaps = ["Character";"Scene Heading"]
    let capitalizeIfWereSupposedTo (typeOfString:string) (x:string) (typesOfThingsWeAlwaysCapitalize:string list) =
        if typesOfThingsWeAlwaysCapitalize |> List.exists (fun x->x=typeOfString) then x.ToUpper() else x        
    let stripSceneTime x = System.Text.RegularExpressions.Regex.Replace(x, " - .+", "")
    let stripDialogContinuationEnding x = System.Text.RegularExpressions.Regex.Replace(x," \(CONT\’D\)$", "")
    let takeFromIndexToNextFind (seqSource:seq<'a>) (index:int) (fnFind:^a->bool) =
        let zapFirstPart = seqSource |> Seq.skip index
        let getRemainingWhileTrue = zapFirstPart |> Seq.takeWhile (fun x->not (fnFind x))
        getRemainingWhileTrue

    let createSceneSubNode (nd:System.Xml.XmlNode) = 
        let newNodeType = getXMLNodeAttributeValueOrEmptyString nd "Type"
        {NodeType=newNodeType; NodeText=(capitalizeIfWereSupposedTo newNodeType nd.InnerText scriptNodeTypesThatShouldAlwaysBeAllCaps)}

    let createSceneFromNode flatList currentIndex (nd:System.Xml.XmlNode):Scene =
        let newTitle =getInnerTextArrayForMatchingXMLNodes nd "Text" |> (putSeparatorIntoStringListWhileConcatentating " \n")
        let scenePropertiesNode = 
            let ret = nd.SelectNodes("SceneProperties")
            if ret.Count>0 then ret.[0] else nd
        let characterArcBeatNodes:System.Xml.XmlNode [] = scenePropertiesNode.SelectNodes("SceneArcBeats/CharacterArcBeat") |> Seq.cast<System.Xml.XmlNode> |> Seq.toArray
        let characterArcBeatList = characterArcBeatNodes |> Seq.cast<System.Xml.XmlNode> |> Seq.map(fun x->getXMLNodeAttributeValueOrEmptyString x "Name") |> Seq.distinct
        let newNodesXML = takeFromIndexToNextFind flatList  (currentIndex+1) (fun x->nodeHasThisAttributeValue x "Type" "Scene Heading")
        let newNodes = newNodesXML |> Seq.toArray |> Array.map(fun x->createSceneSubNode x)
        let newSceneCharacters = newNodes |> Seq.filter(fun x->x.NodeType="Character") |> Seq.map(fun x->stripDialogContinuationEnding x.NodeText) |> Seq.distinct |> Seq.sort |> Seq.toArray |> Array.rev
        let newScriptNotes = getInnerTextArrayForMatchingXMLNodes scenePropertiesNode "ScriptNote/Paragraph"
        let newSceneNotes = getInnerTextArrayForMatchingXMLNodes scenePropertiesNode "Summary/Paragraph"
        let newShots:Shot []=[|{ShotDescription="Default"; Nodes=newNodes; ComputedLabel=""; ComputedDescription=""}|]
        {
            Title=newTitle.ToUpper()
            Length=getXMLNodeAttributeValueOrEmptyString scenePropertiesNode "Length"
            Number=getXMLNodeAttributeValueOrEmptyString nd "Number"
            Page=getXMLNodeAttributeValueOrEmptyString scenePropertiesNode "Page"
            CharacterArcBeats=characterArcBeatList |> Seq.toArray |> Array.sort |> Array.rev
            SceneCharacters=newSceneCharacters
            Notes=newSceneNotes
            Shots=newShots
            ComputedLabel=""
            ComputedDescription=""
        }
    let splitScriptIntoShots (inputScript:Script):Script =
        let createShotFromNode (flatList:SceneSubNode []) shotTitle currentIndex (nd:SceneSubNode):Shot =
            let newTitle =shotTitle
            let newShotNodesXML = takeFromIndexToNextFind flatList  (currentIndex+1) (fun x->x.NodeType = "Shot")
            let newShotNodes = newShotNodesXML |> Seq.map(fun x->
                let newNodeType = x.NodeType
                let newNodeText = x.NodeText
                {NodeType=newNodeType; NodeText=newNodeText}
                )
            {ShotDescription=newTitle; Nodes=newShotNodes |> Seq.toArray; ComputedLabel=""; ComputedDescription=""}
        let newScenes = inputScript.Scenes |> Seq.map(fun x->
            let newScene = x
            let flatList = x.Shots.[0].Nodes
            let newShots = new System.Collections.Generic.List<Shot>()
            flatList |> Seq.iteri(fun i y->
                match i, y.NodeType with
                    | 0,_ ->
                        let shotTitle = "Default" 
                        newShots.Add(createShotFromNode flatList shotTitle (i-1) y) 
                    | _, "Shot" ->
                        let shotTitle = y.NodeText 
                        newShots.Add(createShotFromNode flatList shotTitle i y) 
                    | _,_ -> ()
                )
            {x with Shots=newShots |> Seq.cast<Shot> |> Seq.toArray}
            )
        {Scenes=newScenes |> Seq.toArray}
    let getUniqueScriptLocations (scriptToCheck:Script) =
        let scenesOrderedByLocation = scriptToCheck.Scenes |> Seq.map(fun x->x.Title, x) |> Seq.map(fun (x,y)->(stripSceneTime y.Title),y) |> Seq.sortBy(fun (x,y)->x)
        let scenesGroupedByLocation =scenesOrderedByLocation |> Seq.groupBy(fun (x,y)->x.Trim())
        let newRet = scenesOrderedByLocation |> Seq.groupBy(fun (x,y)->x.Trim()) |> Seq.map(fun (key,scenes)->(key, scenes |> (Seq.map snd) |> Seq.toArray)) |> Seq.toArray
        newRet

    let getDialogForAScene (scene:Scene) =
        scene.Shots.[0].Nodes |> Array.filter(fun x->x.NodeType="Dialog")

    let putSomethingInsideAnHTMLTag (something:string) (tagName:string) (attributes:string) =
        "<" + tagName + attributes + ">" + something + "</" + tagName + ">"

    let sceneNumberOrSceneArrayIndex sceneIndex scene = if scene.Number.Length>0 then scene.Number else sceneIndex.ToString()
    let getLabelAndTitleForAScene (theScene:Scene) (sceneIndex:int) = 
        let sceneNumber = sceneNumberOrSceneArrayIndex sceneIndex theScene
        let anchorValue = "Scene:" + sceneNumber
        let anchorDesc = " " + theScene.Title
        (anchorValue,anchorDesc)
    let getLabelAndTitleForAShot (theScene:Scene) (sceneIndex:int) (theShot:Shot) (shotIndex:int) =
        let sceneNumber = sceneNumberOrSceneArrayIndex sceneIndex theScene
        let anchorValue =  "Shot:" + sceneNumber + ":" + shotIndex.ToString()
        let anchorDesc = if theShot.ShotDescription="Default" then "" else " " + theShot.ShotDescription
        (anchorValue, anchorDesc)
                
    let computeLabelsAndDescriptionsForTheShotsAndScenes (theScript:Script) =
        let newScenes = theScript.Scenes |> Array.mapi(fun i theScene->
            let sceneLabelAndTitle = getLabelAndTitleForAScene theScene i
            let newShots = theScene.Shots |> Array.mapi(fun j theShot->
                let shotLabelAndTitle = getLabelAndTitleForAShot theScene i theShot j
                {theShot with ComputedLabel=(fst shotLabelAndTitle); ComputedDescription=(snd shotLabelAndTitle)}
                )
            {theScene with ComputedLabel=(fst sceneLabelAndTitle); ComputedDescription=(snd sceneLabelAndTitle); Shots=newShots}
            )
        {Scenes=newScenes}


    let createScript (doc:System.Xml.XmlDocument): Script =
        let flatList = doc.SelectNodes("//Content/Paragraph") |> Seq.cast<System.Xml.XmlNode>
        let newScenes = new System.Collections.Generic.List<Scene>()
        flatList |> Seq.iteri(fun i x->
            let nodeTypeAttribute = getXMLNodeAttributeValueOrEmptyString x "Type"
            if nodeTypeAttribute = "Scene Heading" then newScenes.Add(createSceneFromNode flatList i x) else ()
            )
        let tempScript = {Scenes=(newScenes |> Seq.toArray)}
        let splitByShots = splitScriptIntoShots tempScript
        let splitByShotsAndLabelsComputed = computeLabelsAndDescriptionsForTheShotsAndScenes splitByShots
        splitByShotsAndLabelsComputed
