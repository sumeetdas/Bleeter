[<RequireQualifiedAccess>]
module Main

open Elmish
open Feliz
open Tailwind
open Feliz.Router
open Browser.Dom

type State =
    {
        CurrentUrl: string list
        Height: int
        ProfileElem: ProfileElem.State
        Home: Home.State
        DeletedBleet: Bleet option
        SearchBleets: SearchBleets.State
        DistractionBleets: DistractionBleets.State
        SingleBleetPage: SingleBleetPage.State
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
    }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | ProfileElemMsg of ProfileElem.Msg
    | HomeMsg of Home.Msg
    | DataUpdate of Data.State
    | SearchBleetsMsg of SearchBleets.Msg
    | DistractionBleetsMsg of DistractionBleets.Msg
    | SingleBleetPageMsg of SingleBleetPage.Msg

let init (currentUrl: string list) (data: Data.State) : State * Msg Cmd =
    {
        CurrentUrl = currentUrl
        Height = 0
        ProfileElem = ProfileElem.init data
        Home = Home.init data
        DeletedBleet = None
        SearchBleets = SearchBleets.init data
        DistractionBleets = DistractionBleets.init data
        SingleBleetPage = SingleBleetPage.init data
        NotifMsg = None
        ModalMsg = Modal.DoNothing
    },
    Cmd.none

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DataUpdate data ->
        let home, homeCmd = Home.update (Home.Msg.DataUpdate data) state.Home
        let profileElem, profileElemCmd = ProfileElem.update (ProfileElem.Msg.DataUpdate data) state.ProfileElem
        let searchBleets, searchBleetCmd = SearchBleets.update (SearchBleets.Msg.DataUpdate data) state.SearchBleets

        let singleBleetPage, singleBleetPageCmd =
            SingleBleetPage.update (SingleBleetPage.Msg.DataUpdate data) state.SingleBleetPage

        let distractionBleets, distractionBleetsCmd =
            DistractionBleets.update (DistractionBleets.Msg.DataUpdate data) state.DistractionBleets

        { state with
            Home = home
            ProfileElem = profileElem
            SearchBleets = searchBleets
            DistractionBleets = distractionBleets
            SingleBleetPage = singleBleetPage
        },
        Cmd.batch [
            Cmd.map HomeMsg homeCmd
            Cmd.map ProfileElemMsg profileElemCmd
            Cmd.map SearchBleetsMsg searchBleetCmd
            Cmd.map DistractionBleetsMsg distractionBleetsCmd
            Cmd.map SingleBleetPageMsg singleBleetPageCmd
        ]
    | UrlChanged url ->
        let height = document.documentElement.clientHeight |> int

        let state =
            { state with
                Height = height
                ModalMsg = Modal.DoNothing
                NotifMsg = None
            }

        match url with
        | [ "bleeter-info" ] -> { state with CurrentUrl = [ "bleeter-info" ] }, Cmd.none
        | [ "home" ] ->
            let home, homeCmd = Home.update Home.Msg.RefreshHome state.Home
            { state with CurrentUrl = url; Home = home }, (Cmd.map HomeMsg homeCmd)
        | [] ->
            Router.navigate ("home")
            state, Cmd.none
        | [ "search"; (query: string) ] ->
            let searchBleets, searchCmd = SearchBleets.update (SearchBleets.Msg.Search query) state.SearchBleets
            { state with SearchBleets = searchBleets; CurrentUrl = url }, Cmd.map SearchBleetsMsg searchCmd
        | [ "tags"; (tag: string) ] ->
            let distractionBleets, distractionBleetsCmd =
                DistractionBleets.update (DistractionBleets.Msg.LoadTaggedBleets tag) state.DistractionBleets

            { state with
                DistractionBleets = distractionBleets
                CurrentUrl = url
            },
            Cmd.map DistractionBleetsMsg distractionBleetsCmd
        | [ "not-found" ] -> { state with CurrentUrl = url }, Cmd.none
        | [ (handle: string); "bleets"; Route.Int (bleetId: int) ] ->
            let singleBleet, singleBleetCmd =
                SingleBleetPage.update (SingleBleetPage.Msg.LoadBleet(handle, bleetId)) state.SingleBleetPage

            { state with
                SingleBleetPage = singleBleet
                CurrentUrl = url
            },
            Cmd.map SingleBleetPageMsg singleBleetCmd
        | [ (handle: string) ] ->
            let profileElem, cmd = ProfileElem.update (ProfileElem.Msg.UrlChanged handle) state.ProfileElem

            { state with ProfileElem = profileElem; CurrentUrl = url }, Cmd.map ProfileElemMsg cmd
        | _ ->
            Router.navigate ("not-found")
            state, Cmd.none
    | AppHeight height -> { state with Height = height }, Cmd.none
    | ProfileElemMsg msg' ->
        let nextProfileElem, profileCmd = ProfileElem.update msg' state.ProfileElem

        let state =
            { state with
                ProfileElem = nextProfileElem
                NotifMsg = nextProfileElem.NotifMsg
                ModalMsg = nextProfileElem.ModalMsg
            }

        let state = { state with DeletedBleet = nextProfileElem.DeletedBleet }
        state, Cmd.map ProfileElemMsg profileCmd
    | HomeMsg msg' ->
        let nextHome, homeCmd = Home.update msg' state.Home

        { state with
            Home = nextHome
            DeletedBleet = nextHome.DeletedBleet
        },
        Cmd.map HomeMsg homeCmd
    | SearchBleetsMsg msg' ->
        let nextSearchBleets, searchBleetsCmd = SearchBleets.update msg' state.SearchBleets
        { state with SearchBleets = nextSearchBleets }, Cmd.map SearchBleetsMsg searchBleetsCmd
    | DistractionBleetsMsg msg' ->
        let nextDistractionBleets, distractionBleetsCmd = DistractionBleets.update msg' state.DistractionBleets
        { state with DistractionBleets = nextDistractionBleets }, Cmd.map DistractionBleetsMsg distractionBleetsCmd
    | SingleBleetPageMsg msg' ->
        let singleBleet, singleBleetCmd = SingleBleetPage.update msg' state.SingleBleetPage
        { state with SingleBleetPage = singleBleet }, Cmd.map SingleBleetPageMsg singleBleetCmd


let notFoundElem =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
            tw.``text-3xl``
        ]
        prop.text "Page not found"
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-grow-1``
            tw.``max-w-screen-sm``
            tw.``h-full``
            tw.``w-full``
        ]
        prop.style [ style.height state.Height ]
        prop.children [
            match state.CurrentUrl with
            | [ "home" ] -> Home.render state.Home (HomeMsg >> dispatch)
            | [ "bleeter-info" ] -> BleeterInfo.page
            | [ "search"; (_: string) ] -> SearchBleets.render state.SearchBleets (SearchBleetsMsg >> dispatch)
            | [ "tags"; (_: string) ] ->
                DistractionBleets.render state.DistractionBleets (DistractionBleetsMsg >> dispatch)
            | [ "not-found" ] -> notFoundElem
            | [ (_: string); "bleets"; Route.Int (_: int) ] ->
                SingleBleetPage.render state.SingleBleetPage (SingleBleetPageMsg >> dispatch)
            | [ (_: string) ] -> ProfileElem.render state.ProfileElem (ProfileElemMsg >> dispatch)
            | _ -> Html.none
        ]
    ]
