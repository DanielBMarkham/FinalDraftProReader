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
                createReport theScript screenOut
                createReport theScript fileOut
                fileOut.Flush()
                fileOut.Close()
                printfn "Finished"
        runProgram doStuff argv "InOut" "This Program parses Final Draft Pro docs" [|"Help? There is no help"|] "test.fdx" "output.csv"
