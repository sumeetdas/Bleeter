[<RequireQualifiedAccess>]
module ModalPage

open Elmish
open Feliz

type State =
    {
        Display: bool
        CreateBleet: CreateBleet.State
        Meditation: Meditation.State
        CCP: CCP.State
        CurrentUrl: string list
        AddBleet: Bleet option
    }

type Msg =
    | Display
    | CreateBleetMsg of CreateBleet.Msg
    | MeditationMsg of Meditation.Msg
    | CCPMsg of CCP.Msg
    | UrlChanged of string list

let init () =
    {
        Display = false
        CreateBleet = CreateBleet.init ()
        Meditation = Meditation.init ()
        CCP = CCP.init ()
        CurrentUrl = []
        AddBleet = None
    }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | Display -> { state with Display = true }, Cmd.none
    | UrlChanged url ->
        let state = { state with CurrentUrl = url }

        match url with
        | [ "create"; "bleet" ] -> state, Cmd.none
        | [ "modal"; "meditation" ] -> state, Cmd.none
        | [ "modal"; "ccp" ] -> state, Cmd.none
        | _ -> state, Cmd.none
    | CreateBleetMsg msg' ->
        let createBleet, cmd = CreateBleet.update msg' state.CreateBleet

        { state with
            CreateBleet = createBleet
            AddBleet = createBleet.Bleet
        },
        Cmd.map CreateBleetMsg cmd
    | MeditationMsg msg' ->
        let meditation, cmd = Meditation.update msg' state.Meditation
        { state with Meditation = meditation }, Cmd.map MeditationMsg cmd
    | CCPMsg msg' ->
        let ccp, cmd = CCP.update msg' state.CCP
        { state with CCP = ccp }, Cmd.map CCPMsg cmd

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponent =
        match state.CurrentUrl with
        | [ "create"; "bleet" ] -> CreateBleet.render state.CreateBleet (CreateBleetMsg >> dispatch)
        | [ "modal"; "meditation" ] -> Meditation.render state.Meditation (MeditationMsg >> dispatch)
        | [ "modal"; "ccp" ] -> CCP.render state.CCP (CCPMsg >> dispatch)
        | _ -> Html.none

    MainLayout.elem None [ coreComponent ]
