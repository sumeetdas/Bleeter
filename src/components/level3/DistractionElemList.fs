[<RequireQualifiedAccess>]
module DistractionElemList

open Elmish
open Feliz
open Tailwind

type Msg =
    | UrlChanged of string list
    | DataUpdate of Data.State
    | DistractionElemMsg of string * DistractionElem.Msg

type State =
    {
        Data: Data.State
        DistractionElems: DistractionElem.State list
        PreviousUrl: string list
        ModalMsg: Modal.Msg
        NotifMsg: ReactElement option
    }

let init (data: Data.State) =

    {
        Data = data
        DistractionElems = []
        PreviousUrl = []
        ModalMsg = Modal.DoNothing
        NotifMsg = None
    }

let distractionList (state: State) : Distraction list =
    match state.Data.Distractions with
    | Resolved (Ok distractions) -> distractions |> List.truncate 4
    | _ -> []

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | UrlChanged url ->
        let updatedElems =
            state.DistractionElems
            |> List.map
                (fun elem ->
                    let nextElem, _ = DistractionElem.update (DistractionElem.UrlChanged url) elem
                    nextElem)

        { state with
            PreviousUrl = url
            DistractionElems = updatedElems
        },
        Cmd.none
    | DataUpdate data ->
        let nextState = { state with Data = data }

        let distractionElems =
            distractionList nextState
            |> List.map (DistractionElem.init state.PreviousUrl)

        { state with DistractionElems = distractionElems }, Cmd.none
    | DistractionElemMsg (hashTag: string, msg') ->
        let elem =
            state.DistractionElems
            |> List.tryFind (fun elem -> elem.Distraction.Hashtag = hashTag)

        match elem with
        | Some elem ->
            let nextElem, cmd = DistractionElem.update msg' elem

            let updatedElems =
                state.DistractionElems
                |> List.updateAtCustom (fun elem -> elem.Distraction.Hashtag = hashTag) nextElem

            { state with
                DistractionElems = updatedElems
                ModalMsg = nextElem.ModalMsg
                NotifMsg = nextElem.NotifMsg
            },
            Cmd.map (fun msg -> DistractionElemMsg(hashTag, msg)) cmd
        | None -> state, Cmd.none

let distractionElems (state: State) (dispatch: Msg -> unit) =
    state.DistractionElems
    |> List.map
        (fun state ->
            let tag = state.Distraction.Hashtag
            DistractionElem.render state ((fun msg -> DistractionElemMsg(tag, msg)) >> dispatch))

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.``max-w-sm``
            tw.``rounded-lg``
            tw.``bg-gray-100``
            tw.``overflow-hidden``
            tw.``shadow-lg``
            tw.``m-4``
        ]
        prop.children [
            Html.h2 [
                prop.classes [
                    tw.``m-4``
                    tw.``px-4``
                    tw.``py-2``
                    tw.``text-xl``
                    tw.``w-48``
                    tw.``font-semibold``
                ]
                prop.text "Distractions"
            ]
            Html.div [
                prop.children (distractionElems state dispatch)
            ]
        ]
    ]

let renderMobile (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.``w-max``
            tw.``rounded-lg``
            tw.``bg-gray-100``
            tw.``overflow-hidden``
        ]
        prop.children [
            Html.h2 [
                prop.classes [
                    tw.``m-4``
                    tw.``px-4``
                    tw.``py-2``
                    tw.``text-xl``
                    tw.``w-48``
                    tw.``font-semibold``
                ]
                prop.text "Distractions"
            ]
            Html.div [
                prop.children (distractionElems state dispatch)
            ]
        ]
    ]
