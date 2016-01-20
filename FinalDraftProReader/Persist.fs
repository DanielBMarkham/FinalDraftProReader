module Persist
    open Types
    open Utils
    

    let createReport (theScript:Script) (outputWriter:System.IO.TextWriter) =
        let uniquePhysicalLocations = (getUniqueScriptLocations theScript)
        outputWriter.WriteLine ("Physical Locations: " + uniquePhysicalLocations.Length.ToString())
        outputWriter.WriteLine ("Scenes: " + theScript.Scenes.Length.ToString())
        let shotCount = theScript.Scenes |> Seq.sumBy(fun x->x.Shots.Length)
        outputWriter.WriteLine ("Identified Shots: " + shotCount.ToString())
        outputWriter.WriteLine()
        outputWriter.WriteLine "Unique Physical Locations Sorted By Location Complexity".WrapInQuotes
        let uniquePhysicalLocationData = uniquePhysicalLocations |> Seq.map(fun (x,y)->
            let allNodesForThisLocation = y |> Seq.collect(fun m->m.Shots |> Seq.collect(fun n->n.Nodes)) |> Seq.toArray
            let nodeCountForThisLocation = allNodesForThisLocation.Length
            let charactersForThisLocation = allNodesForThisLocation |> Array.filter(fun x->x.NodeType="Character") |> Seq.map(fun x->stripDialogContinuationEnding x.NodeText) |> Seq.distinct |> Seq.sort |> Seq.toArray |> Array.rev
            (x,y,allNodesForThisLocation,nodeCountForThisLocation,charactersForThisLocation)
            )
        let sortedUniquePhysicalLocationData = uniquePhysicalLocationData |> Seq.sortBy( fun(sceneLoc, scene, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->nodeCountForThisLocation) |> Seq.toArray |> Array.rev
        outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|"Set/Location"; "Number Of Scenes"; "Identified Shots"; "Nodes"; "Characters"|])
        sortedUniquePhysicalLocationData |> Seq.iter( fun(sceneLoc, scenes, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->
            let concatCharactersForThisLocation = putSeparatorIntoStringListWhileConcatentating " - " charactersForThisLocation
            let shotCount = scenes |> Seq.sumBy(fun x->x.Shots.Length)
            let line = sceneLoc + "," + scenes.Length.ToString() + ", " + shotCount.ToString() + ", " + (nodeCountForThisLocation.ToString()) + "," + concatCharactersForThisLocation
            outputWriter.WriteLine (line)
            )
        outputWriter.WriteLine()
        outputWriter.WriteLine "Location/Scenes Sorted By Scene Complexity".WrapInQuotes
        outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|"Set/Location"; "Scene Number"; "Starting Page"; "Character List"; "Speaking Segments"; "Notes"|])
        sortedUniquePhysicalLocationData |> Seq.iter( fun(sceneLoc, scenes, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->
            let nowSortBySceneComplexity = scenes |> Array.sortBy(fun x->x.Shots |> Seq.sumBy(fun m->m.Nodes.Length)) |> Array.rev
            nowSortBySceneComplexity |> Seq.iter(fun x->
                let characterList = putSeparatorIntoStringListWhileConcatentating ", " x.SceneCharacters
                let notesConcatenated = putSeparatorIntoStringListWhileConcatentating ", " x.Notes
                let speakingSegments = x.Shots.[0].Nodes.Length.ToString()
                outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|x.Title; x.Number; x.Page; characterList; speakingSegments; notesConcatenated|])
                )
            )
        outputWriter.WriteLine()
        outputWriter.WriteLine "Location-Scene-Shot By Character".WrapInQuotes
        outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|"Set/Location"; "Scene Number"; "Starting Page"; "Shot Number"; "Character List"; "Speaking Segments"; "Notes"|])
        sortedUniquePhysicalLocationData |> Seq.iter( fun(sceneLoc, scenes, allNodesForThisLocation, nodeCountForThisLocation, charactersForThisLocation)->
            let nowSortBySceneComplexity = scenes |> Array.sortBy(fun x->x.Shots |> Seq.sumBy(fun m->m.Nodes.Length)) |> Array.rev
            nowSortBySceneComplexity |> Seq.iter(fun scene->
            let charactersPerShot (theShot:Shot) =
                theShot.Nodes |> Array.filter(fun nd->nd.NodeType="Character") |> Seq.distinct |> Seq.toArray |> Array.map(fun x->x.NodeText)
            let nowSortByCharacterCount = scene.Shots |> Array.sortBy(fun shot->charactersPerShot shot)
            nowSortByCharacterCount |> Seq.iter(fun shot->
                let characterList = putSeparatorIntoStringListWhileConcatentating ", " scene.SceneCharacters
                let notesConcatenated = putSeparatorIntoStringListWhileConcatentating ", " (shot.Nodes |> Array.filter(fun x->x.NodeType="Note") |> Array.map(fun x->x.NodeText))
                let speakingSegments = (shot.Nodes |> Array.filter(fun x->x.NodeType="Dialogue")).Length.ToString()
                let shotTag = shot.ComputedLabel
                outputWriter.WriteLine (putInQuotesAndSeparateByCommas [|scene.Title; scene.Number; scene.Page; shotTag; characterList; speakingSegments; notesConcatenated|])
                )
            )
        )


        let scenesSortedByLocationSceneNumber = theScript.Scenes |> Array.sortBy(fun (x:Scene)->(stripSceneTime x.Title)+x.Number)
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
        outputWriter.WriteLineWithIndent(2,"<link href='mainScript.css' rel='stylesheet' type='text/css' />")
        outputWriter.WriteLineWithIndent(2,putSomethingInsideAnHTMLTag "TEST SCRIPT" "title" "")
        outputWriter.WriteLineWithIndent(2,"<meta name=\"viewport\" content=\"width=600, initial-scale=1\"/>")
        outputWriter.WriteLineWithIndent(1,"</head>")
        outputWriter.WriteLineWithIndent(1,"<body>")
        outputWriter.WriteLineWithIndent(2, "<div class='header'>")
        outputWriter.WriteLineWithIndent(3, "<select class='navSelector' onchange=\" window.location.href = this.options[this.selectedIndex].value; document.getElementById(this.options[this.selectedIndex].value).scrollIntoView(true); \">")

        theScript.Scenes |> Array.iteri(fun i theScene->
            theScene.Shots |> Array.iteri(fun j theShot->
                let anchorValue = if j =0 then theScene.ComputedLabel else theShot.ComputedLabel
                let anchorDesc = if j=0 then theScene.ComputedDescription else theShot.ComputedDescription
                outputWriter.WriteLineWithIndent(4, "<option value='" + "#" + anchorValue + "'>" + anchorValue + anchorDesc + "</option>")                    
                )
            )

        outputWriter.WriteLineWithIndent(3, "</select> <!-- select -->")
        outputWriter.WriteLineWithIndent(2, "</div> <!-- header -->")
        outputWriter.WriteLineWithIndent(2, "<div class='page'>")
        outputWriter.WriteLineWithIndent(3, "<div class='container'>")

        theScript.Scenes |> Array.iteri(fun i theScene->
            outputWriter.WriteLineWithIndent(4,putSomethingInsideAnHTMLTag "" "a" (" id = '" + theScene.ComputedLabel + "" + "' href='#" + theScene.ComputedLabel + "'"))
            outputWriter.WriteLineWithIndent(4,putSomethingInsideAnHTMLTag theScene.ComputedDescription "div" " class='SceneTitle'")
            theScene.Shots |> Array.iteri(fun j theShot->
                outputWriter.WriteLineWithIndent(4,putSomethingInsideAnHTMLTag "" "a" (" id = '" + theShot.ComputedLabel + "' href='#" + theShot.ComputedLabel + "'"))
                if theShot.ShotDescription="Default" 
                    then outputWriter.WriteLineWithIndent(4,putSomethingInsideAnHTMLTag "" "div" " class='ShotDescription'") 
                    else outputWriter.WriteLineWithIndent(4,putSomethingInsideAnHTMLTag theShot.ShotDescription "div" " class='ShotDescription'")
                theShot.Nodes |> Array.iteri(fun k theShotNode->
                    match theShotNode.NodeType with
                        | "Character" -> outputWriter.WriteLineWithIndent(5,putSomethingInsideAnHTMLTag theShotNode.NodeText "div" " class='Character'")
                        | "Dialogue" -> outputWriter.WriteLineWithIndent(5,putSomethingInsideAnHTMLTag theShotNode.NodeText "div" " class='Dialogue'")
                        | "Parenthetical" -> outputWriter.WriteLineWithIndent(5,putSomethingInsideAnHTMLTag theShotNode.NodeText "div" " class='Parenthetical'")
                        | "Action" ->outputWriter.WriteLineWithIndent(5,putSomethingInsideAnHTMLTag theShotNode.NodeText "div" " class='Action'")
                        |_ ->()
                    )
                ()
                )
            ()
            )
        outputWriter.WriteLineWithIndent(3,"</div> <!-- page -->")
        outputWriter.WriteLineWithIndent(3,"</div> <!-- container -->")
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
