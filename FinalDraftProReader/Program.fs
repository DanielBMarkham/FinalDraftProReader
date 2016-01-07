module Main
    open Types
    open Persist
    open Utils

    [<EntryPoint>]
    let main argv = 
        let doStuff (opts:FinalDraftProReaderProgramConfig) =
            if opts.configBase.inputFileExists then
                let xmlDoc = new System.Xml.XmlDocument()
                xmlDoc.Load(opts.configBase.inputFileName.parameterValue)
                let theScript = createScript(xmlDoc)
                let screenOut = System.Console.Out
                let fileOut = System.IO.File.CreateText(opts.configBase.outputFileName.parameterValue)
                let htmlScriptName = "Test"
                let htmlOut = System.IO.File.CreateText(htmlScriptName + ".html")
                createReport theScript screenOut
                createReport theScript fileOut
                createHTMLScript theScript htmlOut
                fileOut.Flush()
                fileOut.Close()
                htmlOut.Flush()
                htmlOut.Close()
                printfn "Finished"
        runProgram doStuff argv "FinalDraftProReader" "This Program parses Final Draft Pro docs" [|"Help? There is no help"|] "input.fdx" "output.csv"
