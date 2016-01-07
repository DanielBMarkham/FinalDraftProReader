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



