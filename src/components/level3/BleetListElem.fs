[<RequireQualifiedAccess>]
module BleetListElem

open Elmish
open Feliz
open Tailwind

type State =
    {
        Bleets: Bleet list
        BleetElems: BleetElem.State list
        ShowLoadMore: bool
        DeletedBleet: Bleet option
        HeightUpdated: bool
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
        PreviousUrl: string list
    }

type Msg =
    | RefreshBleetList
    | LoadMoreBleets of int * int
    | BleetElemMsg of int * BleetElem.Msg
    | DataUpdate of Bleet list
    | UrlChanged of string list

let FETCH_NUM_BLEETS = 10

let init (bleets: Bleet list) =
    {
        Bleets = bleets
        BleetElems = []
        ShowLoadMore = false
        DeletedBleet = None
        HeightUpdated = false
        NotifMsg = None
        ModalMsg = Modal.DoNothing
        PreviousUrl = []
    }

let loadMoreBleets (state: State, currentBleetElemCount: int, numBleetsToLoad: int) =
    let newBleetElemCount = currentBleetElemCount + numBleetsToLoad

    let newBleetElems =
        state.Bleets
        |> List.truncate newBleetElemCount
        |> List.map (BleetElem.init state.PreviousUrl)

    { state with
        BleetElems = newBleetElems
        ShowLoadMore = newBleetElems.Length < state.Bleets.Length
        HeightUpdated = true
    }

let refreshBleetList (state: State) =
    let state = loadMoreBleets (state, 0, FETCH_NUM_BLEETS)
    { state with DeletedBleet = None }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    // clear transient state
    let state = { state with HeightUpdated = false }

    match msg with
    | UrlChanged url ->
        let updatedElems =
            state.BleetElems
            |> List.map
                (fun elem ->
                    let nextElem, _ = BleetElem.update (BleetElem.UrlChanged url) elem
                    nextElem)

        { state with PreviousUrl = url; BleetElems = updatedElems }, Cmd.none
    | DataUpdate bleets ->
        let nextState = refreshBleetList { state with Bleets = bleets }
        nextState, Cmd.none
    | RefreshBleetList -> refreshBleetList state, Cmd.none
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
                    |> List.updateAtCustom (fun elem -> elem.Bleet.Id = id) nextBleetElem

                if nextBleetElem.IsDeleted then
                    { state with
                        BleetElems = newBleetElems
                        DeletedBleet = Some nextBleetElem.Bleet
                        NotifMsg = nextBleetElem.NotifMsg
                        ModalMsg = nextBleetElem.ModalMsg
                    },
                    bleetElemCmd
                else
                    { state with
                        BleetElems = newBleetElems
                        DeletedBleet = None
                        NotifMsg = nextBleetElem.NotifMsg
                        ModalMsg = nextBleetElem.ModalMsg
                    },
                    bleetElemCmd)

let loadMore (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.onClick (fun _ -> dispatch (LoadMoreBleets(state.BleetElems.Length, FETCH_NUM_BLEETS)))
        prop.classes [
            tw.flex
            tw.``flex-row``
            tw.``text-2xl``
            tw.``bg-green-400``
            tw.``text-gray-100``
            tw.``w-full``
            tw.``justify-center``
            tw.``bleeter-pointer``
            tw.``h-16``
            tw.``mt-auto``
            (if state.ShowLoadMore then tw.block else tw.hidden)
        ]

        prop.children [
            Html.button [
                prop.classes [
                    tw.uppercase
                    tw.``bleeter-pointer``
                    tw.``select-none``
                ]
                prop.text (if state.ShowLoadMore then "Load more" else "")
            ]
        ]
    ]


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
            tw.``bg-gray-100``
            tw.``h-full``
        ]
        prop.children (
            [ bleetElemList; [ loadMore state dispatch ] ]
            |> List.concat
        )
    ]
