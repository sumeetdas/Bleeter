[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State =
    {
        Data: Data.State
        Count: int
        BleetElems: BleetElem.State list
    }

type Msg =
    | ClearHomeState
    | GetMoreBleets of int * int
    | LoadMoreBleets of Bleet list
    | BleetElemMsg of int * BleetElem.Msg
    | DataUpdate of Data.State

let init (data: Data.State) = { Data = data; Count = 0; BleetElems = [] }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | DataUpdate data -> 
        match data.Bleets with 
        | Resolved (Ok bleets) -> 
            let newBleetElems = bleets |> List.map BleetElem.init 
            { state with Data = data; BleetElems = newBleetElems }, Cmd.none
        | _ -> { state with Data = data }, Cmd.none
    | ClearHomeState -> init state.Data, Cmd.ofMsg (GetMoreBleets(0, 10))
    | GetMoreBleets (offset, numBleets) ->
        match state.Data.Bleets with
        | Resolved (Ok bleets) ->
            if offset > bleets.Length then
                state, Cmd.none
            else
                let newBleets =
                    bleets
                    |> List.skip offset
                    |> List.take numBleets
                    |> List.map BleetElem.init

                let newBleets = [ state.BleetElems; newBleets ] |> List.concat
                { state with BleetElems = newBleets }, Cmd.none
        | _ -> state, Cmd.none
    | LoadMoreBleets bleets ->
        let newBleetElems = bleets |> List.map BleetElem.init
        let bleetElems = [ state.BleetElems; newBleetElems ] |> List.concat
        { state with BleetElems = bleetElems }, Cmd.none
    | BleetElemMsg (id, msg) ->
        match msg with
        | BleetElem.Msg.ReportBleet ->
            printf "Report"
            state, Cmd.none
        | _ ->
            state.BleetElems
            |> List.tryFind (fun bleetElem -> bleetElem.Bleet.Id = id)
            |> (fun bleetElemOpt ->
                match bleetElemOpt with
                | None -> state, Cmd.none
                | Some bleetElem ->
                    let newBleetElem, cmd = BleetElem.update msg bleetElem

                    let newBleetElems =
                        state.BleetElems
                        |> (List.updateAt (fun elem -> elem.Bleet.Id = id) newBleetElem)

                    { state with BleetElems = newBleetElems }, (Cmd.map (fun msg -> BleetElemMsg(id, msg)) cmd))

let render (state: State) (dispatch: Msg -> unit) =
    let bleetElemList =
        state.BleetElems
        |> List.map
            (fun bleetElem ->
                BleetElem.render
                    bleetElem
                    ((fun msg -> BleetElemMsg(bleetElem.Bleet.Id, msg))
                     >> dispatch))

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
