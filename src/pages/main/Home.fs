[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State =
    {
        Count: int
        BleetElems: Result<BleetElem.State list, string> Deferred
        BleetElems1: BleetElem.State list
    }

type Msg =
    | ClearHomeState
    | GetMoreBleets of int * int
    | LoadMoreBleets of Bleet list
    | BleetElemMsg of int * BleetElem.Msg

let init () = { Count = 0; BleetElems = HasNotStartedYet; BleetElems1 = [] }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | ClearHomeState -> 
        init(), Cmd.ofMsg (GetMoreBleets (0, 10))
    | LoadMoreBleets bleets -> 
        let newBleetElems = bleets |> List.map BleetElem.init
        let bleetElems = [ state.BleetElems1; newBleetElems ] |> List.concat
        { state with BleetElems1 = bleetElems }, Cmd.none
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
    | _ -> 
        state, Cmd.none
        
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
