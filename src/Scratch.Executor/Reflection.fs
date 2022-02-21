namespace Scratch.Executor
open Scratch.Executor.Executor
open Scratch.Primitives
open Scratch


module ExecutionState =
    let stage state = state.mainStage.stageState
    let sprites name state =
        state.mainStage.instances
        |> Seq.filter (fun x -> x.entityImage.entityName = name)
        |> Seq.toList

module EntityState =
    let list listName entity =
        entity.entityImage.listVariables
        |> IArray.toSeqCopiable
        |> Seq.tryFindIndex (fun v -> listName = v.listName)
        |> Option.map (fun index -> entity.listVariables[index] |> Seq.map Value.toSValue |> List.ofSeq)
