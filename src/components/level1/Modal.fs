[<RequireQualifiedAccess>]
module Modal

open Elmish
open Tailwind
open Feliz
open Feliz.Router
// for `?` operator
open Fable.Core.JsInterop

[<RequireQualifiedAccess>]
type ModalType =
    | CreateBleet
    | Meditation
    | CCP
    | Empty

type State =
    {
        Content: ReactElement option
        Display: bool
        ModalId: string
        ModalType: ModalType
        CreateBleet: CreateBleet.State
        Meditation: Meditation.State
        CCP: CCP.State
        PreviousUrl: string list
        NewBleet: Bleet option
    }

type Msg =
    | ShowCreateBleet of Profile * string list
    | ShowMeditation of string list
    | ShowCCP of string list
    | Close
    | OutsideModalClickClose of string
    | CreateBleetMsg of CreateBleet.Msg
    | MeditationMsg of Meditation.Msg
    | CCPMsg of CCP.Msg
    | DoNothing
    | UrlChanged of string list

let init () =
    {
        Content = None
        Display = false
        ModalId = ""
        ModalType = ModalType.Empty
        CreateBleet = CreateBleet.init ()
        Meditation = Meditation.init ()
        CCP = CCP.init ()
        PreviousUrl = []
        NewBleet = None
    }

let closeModal (state: State) =
    let initState = init ()
    printf "modal state.PreviousUrl %A" state.PreviousUrl
    Router.navigate (Router.format(state.PreviousUrl |> List.toArray), HistoryMode.ReplaceState)
    { initState with PreviousUrl = state.PreviousUrl }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | UrlChanged url -> 
        { state with PreviousUrl = url }, Cmd.none
    | ShowCreateBleet (profile, previousUrl) ->
        let createBleet, createBleetCmd = CreateBleet.update (CreateBleet.Display profile) state.CreateBleet

        { state with
            PreviousUrl = previousUrl
            Display = createBleet.Display
            CreateBleet = createBleet
            ModalType = ModalType.CreateBleet
        },
        Cmd.map CreateBleetMsg createBleetCmd
    | ShowMeditation previousUrl ->
        let meditation, meditationCmd = Meditation.update (Meditation.Display) state.Meditation

        { state with
            PreviousUrl = previousUrl
            Display = meditation.Display
            Meditation = meditation
            ModalType = ModalType.Meditation
        },
        Cmd.map MeditationMsg meditationCmd
    | ShowCCP previousUrl ->
        let ccp, ccpCmd = CCP.update (CCP.Display) state.CCP

        { state with
            PreviousUrl = previousUrl
            Display = ccp.Display
            CCP = ccp
            ModalType = ModalType.CCP
        },
        Cmd.map CCPMsg ccpCmd
    | Close -> closeModal state, Cmd.none
    | OutsideModalClickClose id ->
        if (state.Display && id = state.ModalId) then
            match state.ModalType with
            | ModalType.CreateBleet ->
                let createBleet, _ = CreateBleet.update (CreateBleet.OutsideModalClickClose) state.CreateBleet
                let state = { state with CreateBleet = createBleet }

                if not createBleet.Display then
                    closeModal state, Cmd.none
                else
                    state, Cmd.none
            | _ -> state, Cmd.none
        else
            state, Cmd.none
    | CreateBleetMsg msg' ->
        let createBleet, createBleetCmd = CreateBleet.update msg' state.CreateBleet

        { state with
            CreateBleet = createBleet
            NewBleet = createBleet.Bleet
        },
        Cmd.map CreateBleetMsg createBleetCmd
    | MeditationMsg msg' ->
        let meditation, meditationCmd = Meditation.update msg' state.Meditation
        { state with Meditation = meditation }, Cmd.map MeditationMsg meditationCmd
    | _ -> state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let modalClasses =
        [
            tw.``fixed``
            tw.``h-full``
            tw.``w-full``
            tw.``inset-0``
            tw.``z-50``
            tw.``overflow-hidden``
            tw.flex
            tw.``justify-center``
            tw.``items-center``
            tw.``bg-black``
            tw.``bg-opacity-75``
        ]

    let displayModalClass = if state.Display then [ tw.block ] else [ tw.hidden ]

    let classes = [ modalClasses; displayModalClass ] |> List.concat

    let createBleetElem =
        Html.div [
            prop.id state.ModalId
            prop.onClick (fun event -> dispatch (OutsideModalClickClose event.target?id))
            prop.classes classes
            prop.children [
                Html.div [
                    prop.id (state.ModalId + "Main")
                    prop.classes [
                        tw.border
                        tw.``border-blue-500``
                        tw.``shadow-lg``
                        tw.``bg-white``
                        tw.``md:max-w-md``
                        tw.``mx-auto``
                        tw.rounded
                        tw.``shadow-lg``
                        tw.``overflow-y-auto``
                        tw.``text-left``
                        tw.``py-4``
                        tw.``px-6``
                        tw.``w-2/4``
                        tw.``h-48``
                        tw.flex
                        tw.``flex-col``
                    ]
                    prop.children [
                        Html.button [
                            prop.classes [
                                tw.flex
                                tw.``flex-row``
                            ]
                            prop.children [
                                Bleeter.icon "akar-icons:cross" "12"
                            ]
                            prop.onClick (fun _ -> dispatch (Close))
                        ]
                        (match state.ModalType with
                         | ModalType.CreateBleet -> CreateBleet.render state.CreateBleet (CreateBleetMsg >> dispatch)
                         | ModalType.Meditation -> Meditation.render state.Meditation (MeditationMsg >> dispatch)
                         | ModalType.CCP -> CCP.render state.CCP (CCPMsg >> dispatch)
                         | _ -> Html.none)
                    ]
                ]
            ]
        ]

    Html.div [
        prop.classes [ tw.``ml-3`` ]
        prop.children [ createBleetElem ]
    ]
