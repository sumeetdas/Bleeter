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
        ShowLoadMore: bool
        Offset: int
    }

type Msg =
    | AddBleet of Bleet
    | ClearHomeState
    | LoadMoreBleets of int * int
    | BleetElemMsg of int * BleetElem.Msg
    | DataUpdate of Data.State

let FETCH_NUM_BLEETS = 10

let init (data: Data.State) =
    {
        Data = data
        Count = 0
        BleetElems = []
        ShowLoadMore = false
        Offset = 0
    }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | AddBleet bleet ->
        let newBleetElems =
            [ [ (bleet |> BleetElem.init) ]; state.BleetElems ]
            |> List.concat

        { state with BleetElems = newBleetElems }, Cmd.none
    | DataUpdate data -> { state with Data = data }, Cmd.ofMsg ClearHomeState
    | ClearHomeState -> init state.Data, Cmd.ofMsg (LoadMoreBleets(0, FETCH_NUM_BLEETS))
    | LoadMoreBleets (offset, numBleets) ->
        match state.Data.Bleets with
        | Resolved (Ok bleets) ->
            if offset > bleets.Length then
                { state with ShowLoadMore = false }, Cmd.none
            else
                let newBleets =
                    bleets
                    |> List.skip offset
                    |> List.truncate numBleets
                    |> List.map BleetElem.init

                let newBleets = [ state.BleetElems; newBleets ] |> List.concat

                { state with
                    BleetElems = newBleets
                    ShowLoadMore = true
                    Offset = offset + FETCH_NUM_BLEETS
                },
                Cmd.none
        | _ -> state, Cmd.none
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

    let loadMore =
        Html.div [
            prop.classes [
                tw.``text-3xl``
                tw.``m-auto``
            ]
            prop.text "Load more"
        ]

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
                prop.onClick (fun _ -> dispatch (LoadMoreBleets(state.Offset, FETCH_NUM_BLEETS)))
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                ]
                prop.children (
                    [
                        bleetElemList
                        [ (if state.ShowLoadMore then loadMore else Html.none) ]
                    ]
                    |> List.concat
                )
            ]
        ]
    ]
