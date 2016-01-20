﻿module Types
    ///
    /// .NET Type Extensions
    ///
    type 'a ``[]`` with 
        member x.randomItem = 
            let rnd = new System.Random()
            let idx = rnd.Next(x.Length)
            x.[idx]
    type System.Xml.XmlNode with
        member x.GetConcatenatedValuesForAName (childName:string) (concatString:string) =
            if not x.HasChildNodes then ""
            else
                let xPath = "//" + childName
                let matchingNodes = x.SelectNodes(xPath)
                if matchingNodes = null || matchingNodes.Count = 0 then ""
                else
                    let concatLength = concatString.Length
                    let ret = matchingNodes |> Seq.cast<System.Xml.XmlNode> |> Seq.map(fun x->x.Value + concatString) |> Seq.fold(fun acc x->acc + x) ""
                    let retWithTrim = if ret.Length>concatLength then ret.Substring(0, ret.Length-concatLength) else ""
                    retWithTrim
    type System.String with
        member x.ContainsAny (possibleMatches:string[]) =
            let ret = possibleMatches |> Array.tryFind(fun y->
                x.Contains(y)
                )
            ret.IsSome
        member x.ContainsAnyRegex(possibleRegexMatches:string[]) =
            let ret = possibleRegexMatches |> Array.tryFind(fun y->
                let rg = new System.Text.RegularExpressions.Regex(y)
                rg.IsMatch(x)
                )
            ret.IsSome
        member x.ContainsRegex(regexMatch:string) =
            let rg = new System.Text.RegularExpressions.Regex(regexMatch)
            rg.IsMatch(x)
        member x.ReplaceWithRegex (regexMatchString:string) (replacementString:string) = 
            System.Text.RegularExpressions.Regex.Replace(x, regexMatchString, replacementString)
        member x.CountOccurences (token:string) = 
            let mts = x.Split([|token|], System.StringSplitOptions.None)
            if mts = null then 0 else mts.Length
        member x.CountOccurencesRegex (regexMatchString:string) =
            let mts = System.Text.RegularExpressions.Regex.Matches(x, regexMatchString)
            if mts = null then 0 else mts.Count
        member this.GetRight (iLen:int) =
            try
                this.Substring(this.Length - iLen, iLen)
            with |_ -> ""
        member this.GetLeft (iLen:int) =
            try
                this.Substring(0, iLen)
            with |_ -> ""
        member this.TrimLeft (iCount:int) =
            if iCount >= this.Length then
                this.Substring(iCount, this.Length - iCount)
            else ""
        member this.TrimRight (iCount:int) =
            if iCount >= this.Length then
                this.Substring(0, this.Length - iCount)    
            else
                ""
        member this.TrimBoth (iLeft:int) (iRight:int) =
            if iLeft + iRight > this.Length
                then
                    ""
                else
                    (this.TrimLeft iLeft) |> (fun x-> x.TrimRight iRight)
        member this.TrimTo (desiredLength:int) =
            if this.Length <= desiredLength
                then
                    this
                else
                    this.GetLeft desiredLength
        member this.WrapInQuotes =
            ("\"" + this + "\"")

    type System.Collections.Generic.Dictionary<'A, 'B> with
        member x.stringValueOrEmptyForKey n = 
            if x.ContainsKey n then x.Item(n).ToString() else ""
        member x.TryFind n = 
            let x,(y:'B) = x.TryGetValue n
            if x then Some y else None
    type System.Text.RegularExpressions.MatchCollection with
        member this.toSeq =
            seq {for i = 0 to this.Count - 1 do yield this.[i]}
        member this.toArray =
            [|for i = 0 to this.Count - 1 do yield this.[i] |]
    type System.Text.RegularExpressions.Match with
        member this.lastGroup =
            this.Groups.[this.Groups.Count-1]
        member this.lastIndex =
            this.lastGroup.Index + this.lastGroup.Length

    //
    // Types specific to this family of programs
    //

    exception UserNeedsHelp of string
    exception ExpectedResponseFail of string
    type Verbosity =
        | Silent            = 1
        | BatchMinimum      = 2
        | Minimum           = 3
        | BatchNormal       = 4
        | Normal            = 5
        | BatchVerbose      = 6
        | Verbose           = 7
    let getMatchingParameters (args:string []) (symbol:string) = 
        args |> Array.filter(fun x->
                    let argParms = x.Split([|':'|],2)
                    let parmName = (argParms.[0]).Substring(1).ToUpper()
                    if argParms.Length > 0 then parmName=symbol.ToUpper() else false
                    )
    let getValuePartOfMostRelevantCommandLineMatch (args:string []) (symbol:string) =
        let matchingParms = getMatchingParameters args symbol
        if matchingParms.Length > 0
            then
                // if there are multiple entries, last one overrides the rest
                let commandLineParm = matchingParms.[matchingParms.Length-1]
                let parmSections=commandLineParm.Split([|':'|], 2)
                if parmSections.Length<2 then Some "" else Some parmSections.[1]
            else
                None
    type ConfigEntry<'A> =
        {
            commandLineParameterSymbol:string
            commandLineParameterName:string
            parameterHelpText:string[]
            parameterValue:'A
        } with
            member this.printVal =
                printfn "%s: %s" this.commandLineParameterName (this.parameterValue.ToString())
            member this.printHelp =
                printfn "/%s" this.commandLineParameterSymbol
                printfn "    %s" this.commandLineParameterName
                this.parameterHelpText |> Seq.iter(printfn "    %s")
            member this.swapInNewValue x =
                {this with parameterValue=x}
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Verbosity>), (args:string[])):ConfigEntry<Verbosity>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parsedNumValue = System.Int32.Parse("0" + parmValue.Value)
                        let parsedVerbosityValue = enum<Verbosity>(parsedNumValue)
                        defaultConfig.swapInNewValue parsedVerbosityValue
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<string>), (args:string[])):ConfigEntry<string>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue parmValue.Value
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<bool>), (args:string[])):ConfigEntry<bool> =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        if parmValue.Value.ToUpper() = "FALSE" || parmValue.Value = "0" || parmValue.Value.ToUpper() = "F" || parmValue.Value.ToUpper() = "NO"
                            then
                                defaultConfig.swapInNewValue false
                            else
                                defaultConfig.swapInNewValue true
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<int>), (args:string[])):ConfigEntry<int>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parmInt = System.Int32.Parse("0" + parmValue.Value)
                        defaultConfig.swapInNewValue parmInt
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.Uri>), (args:string[])):ConfigEntry<System.Uri>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (new System.Uri(parmValue.Value))
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.DateTime>), (args:string[])):ConfigEntry<System.DateTime>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (System.DateTime.Parse(parmValue.Value))
                    else
                        defaultConfig
    type InterimProgress =
        {
            items:System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>
        } with
        member this.addItem key (vl:string) =
            let lookup = 
                if this.items.ContainsKey key then this.items.Item(key)
                    else
                        let newItem = new System.Text.StringBuilder(65535)
                        this.items.Add(key,newItem)
                        newItem
            lookup.Append("\r\n" + vl) |> ignore
        member this.getItem key  =
            if this.items.ContainsKey key
                then
                    this.items.Item(key).ToString()
                else
                    ""
    type ConfigBase =
        {
            programName:string
            programTagLine:string
            programHelpText:string[]
            verbose:ConfigEntry<Verbosity>
            interimProgress:InterimProgress
            inputFileName:ConfigEntry<string>
            outputFileName:ConfigEntry<string>
            inputFileExists:bool
            outputFileExists:bool
        }
        member this.printProgramDescription =
            this.programHelpText |> Seq.iter(System.Console.WriteLine)
        member this.printThis =
            printfn "%s" this.programName
            this.programHelpText |> Seq.iter(printfn "    %s")
            this.inputFileName.printVal
            printfn "inputFileExists: %b" this.inputFileExists
            this.outputFileName.printVal
            printfn "outputFileExists: %b" this.outputFileExists
        member this.printHelp =
            this.inputFileName.printHelp
            this.outputFileName.printHelp
            this.verbose.printHelp

    ///
    /// Types specific to this program
    ///

    type FinalDraftProReaderProgramConfig =
        {
            configBase:ConfigBase
            // Add program-specific variables here
            dummyVariable:string
        }
        member this.printThis() =
            printfn "Config Parameters Provided"
            this.configBase.verbose.printVal
            printfn "dummyVariable: %s" this.dummyVariable
        member this.printCommandLineParameterHelp() =
            printfn "%s" this.configBase.programName
            printfn "========================"
            printfn "Command Line Options:"
            this.configBase.printHelp


    type SceneSubNode =
        {   NodeType:string; 
            NodeText:string
        }
    type Shot =
        {
            ShotDescription:string
            Nodes:SceneSubNode []
            ComputedLabel:string
            ComputedDescription:string
        }
    type Scene =  
        {   Length:string; 
            Number: string; 
            Page:string; 
            Title:string; 
            Notes: string []; 
            CharacterArcBeats:string []; 
            SceneCharacters:string []; 
            Shots:Shot [];
            ComputedLabel:string
            ComputedDescription:string
        }
    type Script = 
        { 
            Scenes: Scene []
        }
