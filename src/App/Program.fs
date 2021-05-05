open System
open Library

[<EntryPoint>]
let main argv =

    let numNodesStr = argv.[0]
    let numNodes =
        match Int32.TryParse numNodesStr with
        | true, int -> int
        | _ -> failwith "numNodes should be integer"
    let topology = argv.[1]
    let algorithm = argv.[2]
    
    // For testing
    // GossipSimulator.start 4 "Full" "Gossip"
    // GossipSimulator.start 4 "Line" "Gossip"
    // GossipSimulator.start 4 "2DGrid" "Gossip"
    // GossipSimulator.start 4 "Imp2DGrid" "Gossip"
    // GossipSimulator.start 4 "Full" "PushSum"
    // GossipSimulator.start 4 "Line" "PushSum"
    // GossipSimulator.start 4 "2DGrid" "PushSum"
    // GossipSimulator.start 4 "Imp2DGrid" "PushSum"

    GossipSimulator.start numNodes topology algorithm
    
    printfn "finished!"
    // For testing
    // Async.Sleep 5000 |> Async.RunSynchronously

    0 // return an integer exit code
