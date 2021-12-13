[<RequireQualifiedAccess>]
module MobilePage

open Elmish
open Feliz
open Feliz.Router
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

let updateDistractionList (msg: DistractionElemList.Msg) (state: State) =
    let distraction, cmd = DistractionElemList.update msg state.DistractionElemList
    { state with DistractionElemList = distraction }, Cmd.map DistractionMsg cmd

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
        let state = { state with Data = data }
        let state, _ = updateDistractionList (DistractionElemList.DataUpdate data) state

        match state.Data.MyProfile with
        | Some profile -> updateCreateBleet (CreateBleet.DataUpdated profile) state
        | None -> state, Cmd.none
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
    | DistractionMsg msg' -> updateDistractionList msg' state
    | SearchBoxMsg msg' ->
        let searchBox = SearchBox.update msg' state.SearchBox
        let state = { state with SearchBox = searchBox }

        if searchBox.DoSearch then
            Router.navigate ("search", searchBox.Content)
            let searchBox = SearchBox.update SearchBox.Msg.Clear state.SearchBox
            { state with SearchBox = searchBox }, Cmd.none
        else
            state, Cmd.none


let render (state: State) (dispatch: Msg -> unit) =
    let coreComponent =
        match state.CurrentUrl with
        | [ "create"; "bleet" ] -> CreateBleet.render state.CreateBleet (CreateBleetMsg >> dispatch)
        | [ "modal"; "meditation" ] -> Meditation.render state.Meditation (MeditationMsg >> dispatch)
        | [ "modal"; "ccp" ] -> CCP.render state.CCP (CCPMsg >> dispatch)
        | [ "explore" ] -> DistractionElemList.renderMobile state.DistractionElemList (DistractionMsg >> dispatch)
        | [ "search" ] -> SearchBox.render state.SearchBox (SearchBoxMsg >> dispatch)
        | _ -> Html.none

    MainLayout.mobileElem (Some "/img/bleeter-logo.png") [ coreComponent ]
