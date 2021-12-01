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
        (asyncOpStatus: Result<'b, string> AsyncOperationStatus)
        (jsonUrl: string)
        (parseJson: string -> Result<'b, string>)
        (resultToFinishedMsg: Result<'b, string> -> 'msg)
        (toInProgressState: 'state -> 'state)
        (toResolvedState: 'state * Result<'b, string> -> 'state)
        (commandAfterFinished: 'msg Cmd)
        : 'state * 'msg Cmd =

        match asyncOpStatus with
        | Started ->
            let nextState = toInProgressState state

            let loadCmd =
                async {
                    let! (statusCode, json) = jsonUrl |> Http.get

                    if statusCode = 200 then
                        return json |> parseJson |> resultToFinishedMsg
                    else
                        return Error "error while fetching profile" |> resultToFinishedMsg
                }

            nextState, Cmd.fromAsync loadCmd
        | Finished result ->
            let nextState = toResolvedState (state, result)
            nextState, commandAfterFinished
