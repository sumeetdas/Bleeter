[<AutoOpen>]
module AsyncTypes

open Elmish
open Fable.SimpleHttp

type 't Deferred =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type 't AsyncOperationStatus =
    | Started
    | Finished of 't

[<RequireQualifiedAccess>]
module AsyncOperationStatus =
    let loadData
        (state: 'state)
        (asyncOpStatus: Result<'b list, string> AsyncOperationStatus)
        (jsonUrl: string)
        (jsonToList: string -> Result<'b list, string>)
        (resultToMsg: Result<'b list, string> -> 'msg)
        (toInProgressState: 'state -> 'state)
        (toResolvedState: 'state * Result<'b list, string> -> 'state)
        : 'state * 'msg Cmd =

        match asyncOpStatus with
        | Started ->
            let nextState = toInProgressState state

            let loadCmd =
                async {
                    let! (statusCode, json) = jsonUrl |> Http.get

                    if statusCode = 200 then
                        return json |> jsonToList |> resultToMsg
                    else
                        return Error "error while fetching profile" |> resultToMsg
                }

            nextState, Cmd.fromAsync loadCmd
        | Finished result ->
            let nextState = toResolvedState (state, result)
            nextState, Cmd.none
