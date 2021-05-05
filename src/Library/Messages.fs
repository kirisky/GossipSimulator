namespace Library.Messages

open Akka.Actor

[<AutoOpen>]
module Messages =

    type GossipMessage =
    | GossipRumour
    | GossipInitiateNeighBours of List<IActorRef>

    type PushSumMessage =
    | PushSumRumour of float * float
    | PushSumInitiateNeighBours of List<IActorRef>

    type AlgorithmGossipStatus =
    | GossipContinue
    | GossipExit
    | GossipIgnore

    type AlgorithmPushSumStatus =
    | PushSumContinue of int
    | PushSumExit
    | PushSumIgnore

    type AlgorithmType =
    | GossipAlgo
    | PushSumAlgo

    type ResultType =
    | Done

    let (|GossipAlgo|PushSumAlgo|) (name: string) =
        if name.ToUpper() = "Gossip".ToUpper() then GossipAlgo
        elif name.ToUpper() = "PushSum".ToUpper() then PushSumAlgo
        else failwith "Unknown type of algorithm"
    
    type TopologyTypes =
    | TopoFull
    | TopoLine
    | Topo2DGrid
    | TopoImperfect2DGrid

    let (|TopoFull|TopoLine|Topo2DGrid|TopoImperfect2DGrid|) (name: string) =
        if name.ToUpper() = "Full".ToUpper() then TopoFull
        elif name.ToUpper() = "Line".ToUpper() then TopoLine
        elif name.ToUpper() = "2DGrid".ToUpper() then Topo2DGrid
        elif name.ToUpper() = "Imp2DGrid".ToUpper() then TopoImperfect2DGrid
        else failwith "Unknown type of topology"
