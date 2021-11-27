[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind
open Fable.SimpleHttp

type State =
    {
        Count: int
        BleetElems: Result<BleetElem.State list, string> Deferred
    }

type Msg =
    | BleetElemMsg of int * BleetElem.Msg
    | LoadBleets of Result<BleetElem.State list, string> AsyncOperationStatus

let init () = { Count = 0; BleetElems = HasNotStartedYet }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | BleetElemMsg (id, msg) ->
        match msg with
        | BleetElem.Msg.ReportBleet ->
            printf "Report"
            state, Cmd.none
        | _ ->
            match state.BleetElems with
            | Resolved (Ok bleets) ->
                bleets
                |> List.tryFind (fun bleetElem -> bleetElem.Bleet.Id = id)
                |> (fun bleetElemOpt ->
                    match bleetElemOpt with
                    | None -> state, Cmd.none
                    | Some bleetElem ->
                        let newBleetElem, cmd = BleetElem.update msg bleetElem

                        let newBleetElems =
                            bleets
                            |> (List.updateAt (fun elem -> elem.Bleet.Id = id) newBleetElem)

                        { state with BleetElems = Resolved(Ok newBleetElems) },
                        (Cmd.map (fun msg -> BleetElemMsg(id, msg)) cmd))
            | _ -> state, Cmd.none
    | LoadBleets asyncOpStatus ->
        match asyncOpStatus with
        | Started ->
            let nextState = { state with BleetElems = InProgress }

            let loadProfileCmd =
                async {
                    let! (statusCode, bleets) = "/data/Bleets.json" |> Http.get

                    if statusCode = 200 then
                        return
                            LoadBleets(
                                Finished(
                                    bleets
                                    |> Bleet.decodeListResult
                                    |> (fun result ->
                                        match result with
                                        | Ok bleetList -> bleetList |> List.map BleetElem.init |> Ok
                                        | Error error -> Error error)
                                )
                            )
                    else
                        return LoadBleets(Finished(Error "error while fetching profile"))
                }

            nextState, Cmd.fromAsync loadProfileCmd
        | Finished bleets ->
            let nextState = { state with BleetElems = Resolved bleets }
            nextState, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let bleetElemList =
        match state.BleetElems with
        | Resolved (Ok bleets) ->
            bleets
            |> List.map
                (fun bleetElem ->
                    BleetElem.render
                        bleetElem
                        ((fun msg -> BleetElemMsg(bleetElem.Bleet.Id, msg))
                         >> dispatch))
        | Resolved (Error err) ->
            printf "%A" err
            []
        | _ -> []

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
        ]
        prop.children [
            Html.div [
                prop.classes [ tw.flex; tw.``h-20`` ]
                prop.text "Home"
            ]
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                ]
                prop.children bleetElemList
            ]
        ]
    ]
