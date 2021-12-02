[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State =
    {
        Data: Data.State
        BleetElems: BleetElem.State list
        ShowLoadMore: bool
        DeletedBleet: Bleet option
    }

type Msg =
    | ClearHomeState
    | LoadMoreBleets of int * int
    | BleetElemMsg of int * BleetElem.Msg
    | DataUpdate of Data.State

let FETCH_NUM_BLEETS = 10

let init (data: Data.State) =
    {
        Data = data
        BleetElems = []
        ShowLoadMore = false
        DeletedBleet = None
    }

let loadMoreBleets (state: State, currentBleetElemCount: int, numBleetsToLoad: int) =
    match state.Data.Bleets with
    | Resolved (Ok bleets) ->
        let newBleetElemCount = currentBleetElemCount + numBleetsToLoad

        let newBleetElems =
            bleets
            |> List.truncate newBleetElemCount
            |> List.map BleetElem.init

        { state with
            BleetElems = newBleetElems
            ShowLoadMore = newBleetElems.Length < bleets.Length
        }
    | _ -> state

let clearHomeState (state: State) =
    let state = loadMoreBleets (state, 0, FETCH_NUM_BLEETS)
    { state with DeletedBleet = None }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | DataUpdate data ->
        let nextState = clearHomeState { state with Data = data }
        nextState, Cmd.none
    | ClearHomeState -> clearHomeState state, Cmd.none
    | LoadMoreBleets (currentBleetElemCount, numBleetsToLoad) ->
        loadMoreBleets (state, currentBleetElemCount, numBleetsToLoad), Cmd.none
    | BleetElemMsg (id, msg') ->
        state.BleetElems
        |> List.tryFind (fun bleetElem -> bleetElem.Bleet.Id = id)
        |> (fun bleetElemOpt ->
            match bleetElemOpt with
            | None -> state, Cmd.none
            | Some bleetElem ->
                let nextBleetElem, bleetElemCmd = BleetElem.update msg' bleetElem
                let bleetElemCmd = Cmd.map (fun msg -> BleetElemMsg(id, msg)) bleetElemCmd

                let newBleetElems =
                    state.BleetElems
                    |> List.updateAt (fun elem -> elem.Bleet.Id = id) nextBleetElem

                if nextBleetElem.IsDeleted then
                    { state with
                        BleetElems = newBleetElems
                        DeletedBleet = Some nextBleetElem.Bleet
                    },
                    bleetElemCmd
                else
                    { state with BleetElems = newBleetElems; DeletedBleet = None }, bleetElemCmd)

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
            prop.onClick (fun _ -> dispatch (LoadMoreBleets(state.BleetElems.Length, FETCH_NUM_BLEETS)))
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
