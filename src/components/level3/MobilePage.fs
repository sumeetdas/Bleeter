[<RequireQualifiedAccess>]
module MobilePage

open Elmish
open Feliz
open Tailwind

type State =
    {
        Data: Data.State
        Display: bool
        CreateBleet: CreateBleet.State
        Meditation: Meditation.State
        CCP: CCP.State
        CurrentUrl: string list
        AddBleet: Bleet option
        DistractionElemList: DistractionElemList.State
        SearchBox: SearchBox.State
    }

type Msg =
    | Display
    | CreateBleetMsg of CreateBleet.Msg
    | MeditationMsg of Meditation.Msg
    | CCPMsg of CCP.Msg
    | UrlChanged of string list
    | DistractionMsg of DistractionElemList.Msg
    | SearchBoxMsg of SearchBox.Msg
    | DataUpdated of Data.State

let init (data: Data.State) =
    {
        Data = data
        Display = false
        CreateBleet = CreateBleet.init ()
        Meditation = Meditation.init ()
        CCP = CCP.init ()
        CurrentUrl = []
        AddBleet = None
        DistractionElemList = DistractionElemList.init data
        SearchBox = SearchBox.init ()
    }

let updateCreateBleet (msg: CreateBleet.Msg) (state: State) = 
    let createBleet, cmd = CreateBleet.update msg state.CreateBleet

    { state with
        CreateBleet = createBleet
        AddBleet = createBleet.Bleet
    },
    Cmd.map CreateBleetMsg cmd

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DataUpdated data -> 
        { state with Data = data }, Cmd.none
    | Display -> { state with Display = true }, Cmd.none
    | UrlChanged url ->
        let state = { state with CurrentUrl = url }

        match url with
        | [ "create"; "bleet" ] -> 
            let state, _ = updateCreateBleet (CreateBleet.Display) state 
            match state.Data.MyProfile with 
            | Some profile -> updateCreateBleet (CreateBleet.DataUpdated profile) state 
            | None -> state, Cmd.none 
        | [ "modal"; "meditation" ] -> state, Cmd.none
        | [ "modal"; "ccp" ] -> state, Cmd.none
        | [ "explore" ] -> state, Cmd.none
        | [ "search" ] -> state, Cmd.none
        | _ -> state, Cmd.none
    | CreateBleetMsg msg' -> updateCreateBleet msg' state 
    | MeditationMsg msg' ->
        let meditation, cmd = Meditation.update msg' state.Meditation
        { state with Meditation = meditation }, Cmd.map MeditationMsg cmd
    | CCPMsg msg' ->
        let ccp, cmd = CCP.update msg' state.CCP
        { state with CCP = ccp }, Cmd.map CCPMsg cmd
    | DistractionMsg msg' ->
        let distraction, cmd = DistractionElemList.update msg' state.DistractionElemList
        { state with DistractionElemList = distraction }, Cmd.map DistractionMsg cmd
    | SearchBoxMsg msg' -> 
        let searchBox = SearchBox.update msg' state.SearchBox
        { state with SearchBox = searchBox }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let wrapper (elem: ReactElement) = 
        Html.div [
            prop.classes [ 
                tw.``flex``
                tw.``flex-col``
                tw.``m-4`` 
                tw.``w-full``
            ]
            prop.children [ elem ]
        ]

    let coreComponent =
        match state.CurrentUrl with
        | [ "create"; "bleet" ] -> wrapper (CreateBleet.render state.CreateBleet (CreateBleetMsg >> dispatch))
        | [ "modal"; "meditation" ] -> wrapper (Meditation.render state.Meditation (MeditationMsg >> dispatch))
        | [ "modal"; "ccp" ] -> wrapper (CCP.render state.CCP (CCPMsg >> dispatch))
        | [ "explore" ] -> wrapper (DistractionElemList.render state.DistractionElemList (DistractionMsg >> dispatch))
        | [ "search" ] -> wrapper (SearchBox.render state.SearchBox (SearchBoxMsg >> dispatch))
        | _ -> Html.none

    Html.div [
        prop.classes [ tw.``m-4`` ]
        prop.children [
            MainLayout.elem (Some "/img/bleeter-logo.png") [ coreComponent ]
        ]
    ]
    
