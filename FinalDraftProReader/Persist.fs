module Persist
    open Types
    open Utils
    

    let createReport (theScript:Script) (outputWriter:System.IO.TextWriter) =
        outputWriter.WriteLine ("Scene Count: " + theScript.Scenes.Length.ToString())
        outputWriter.WriteLine()
        let uniquePhysicalLocations = (getUniqueScriptLocations theScript)
        outputWriter.WriteLine "Unique Physical Locations".WrapInQuotes
        let uniquePhysicalLocationData = uniquePhysicalLocations |> Seq.map(fun (x,y)->
            let allNodesForThisLocation = y |> Seq.collect(fun m->m.Shots.[0].Nodes) |> Seq.toArray
            let nodeCountForThisLocation = allNodesForThisLocation.Length
            let charactersForThisLocation = allNodesForThisLocation |> Array.filter(fun x->x.NodeType="Character") |> Seq.map(fun x->stripDialogContinuationEnding x.NodeText) |> Seq.distinct |> Seq.sort |> Seq.toArray |> Array.rev
            (x,y,allNodesForThisLocation,nodeCountForThisLocation,charactersForThisLocation)
            )
        let sortedUniquePhysicalLocationData = uniquePhysicalLocationData |> Seq.sortBy( fun(sceneLoc, scene, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->nodeCountForThisLocation) |> Seq.toArray |> Array.rev
        outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|"Set/Location"; "Number Of Nodes"; "Characters"|])
        sortedUniquePhysicalLocationData |> Seq.iter( fun(sceneLoc, scene, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->
            let concatCharactersForThisLocation = putSeparatorIntoStringListWhileConcatentating " - " charactersForThisLocation
            let line = sceneLoc + "," + (nodeCountForThisLocation.ToString()) + "," + concatCharactersForThisLocation
            outputWriter.WriteLine (line)
            )
        outputWriter.WriteLine()
        outputWriter.WriteLine "Sorted Shot List (By Location, Scene Number)".WrapInQuotes
        let scenesSortedByLocationSceneNumber = theScript.Scenes |> Array.sortBy(fun (x:Scene)->(stripSceneTime x.Title)+x.Number)
        outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|"Set/Location"; "Scene Number"; "Starting Page"; "Character List"; "Speaking Segments"; "Notes"|])
        scenesSortedByLocationSceneNumber |> Array.iter(fun x->
            let characterList = putSeparatorIntoStringListWhileConcatentating ", " x.SceneCharacters
            let notesConcatenated = putSeparatorIntoStringListWhileConcatentating ", " x.Notes
            let speakingSegments = x.Shots.[0].Nodes.Length.ToString()
            outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|x.Title; x.Number; x.Page; characterList; speakingSegments; notesConcatenated|])
            )
        outputWriter.WriteLine()
        ()

    type System.IO.TextWriter with
        member x.WriteLineWithIndent(iLevel, (line:string)) =
            x.WriteLine(("".PadLeft(iLevel*2)) + line)
    let createHTMLScript (theScript:Script) (outputWriter:System.IO.TextWriter) =
        outputWriter.WriteLine("<!DOCTYPE html>")
        outputWriter.WriteLine("<html>")
        outputWriter.WriteLineWithIndent(1, "<head>")
        outputWriter.WriteLineWithIndent(2,"<meta charset=\"utf-8\">")
        outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag "" "link" " href='..\..\mainScript.css' rel='stylesheet' type='text/css'")
        outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag "TEST SCRIPT" "title" "")
        outputWriter.WriteLineWithIndent(1,"</head>")
        outputWriter.WriteLineWithIndent(1,"<body>")
        theScript.Scenes |> Array.iteri(fun i x->
            outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag "" "a" (" href='#Scene:" + x.Number + "'"))
            outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag x.Title "div" " class='SceneTitle'")
            x.Shots |> Array.iteri(fun j y->
                if y.ShotDescription="Default" then () else outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag y.ShotDescription "div" " class='ShotDescription'")
                y.Nodes |> Array.iteri(fun k z->
                    match z.NodeType with
                        | "Character" -> outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag z.NodeText "div" " class='Character'")
                        | "Dialogue" -> outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag z.NodeText "div" " class='Dialogue'")
                        | "Parenthetical" -> outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag z.NodeText "div" " class='Parenthetical'")
                        |_ ->()
                    )
                ()
                )
            ()
            )

        outputWriter.WriteLine("<script>")
        outputWriter.WriteLine("")
        outputWriter.WriteLine("  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){")
        outputWriter.WriteLine("  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),")
        outputWriter.WriteLine("  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)")
        outputWriter.WriteLine("  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');")
        outputWriter.WriteLine("")
        outputWriter.WriteLine("  ga('create', 'UA-72221858-1', 'auto');")
        outputWriter.WriteLine("  ga('send', 'pageview');")
        outputWriter.WriteLine("")
        outputWriter.WriteLine("</script>")

        outputWriter.WriteLine("</body>")
        outputWriter.WriteLine("</html>")
        ()
