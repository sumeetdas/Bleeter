[<RequireQualifiedAccess>]
module Main

open Elmish
open Feliz
open Tailwind
open Feliz.Router

type State =
    {
        CurrentUrl: string list
        Height: int
        ProfileElem: ProfileElem.State
        Home: Home.State
        DeletedBleet: Bleet option
        SearchBleets: SearchBleets.State
    }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | ProfileElemMsg of ProfileElem.Msg
    | HomeMsg of Home.Msg
    | DataUpdate of Data.State
    | SearchBleetsMsg of SearchBleets.Msg

let init (currentUrl: string list) (data: Data.State) : State * Msg Cmd =
    {
        CurrentUrl = currentUrl
        Height = 0
        ProfileElem = ProfileElem.init data
        Home = Home.init data
        DeletedBleet = None
        SearchBleets = SearchBleets.init data
    },
    Cmd.none

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DataUpdate data ->
        let home, homeCmd = Home.update (Home.Msg.DataUpdate data) state.Home
        let profileElem, profileElemCmd = ProfileElem.update (ProfileElem.Msg.DataUpdate data) state.ProfileElem
        let searchBleets, searchBleetCmd = SearchBleets.update (SearchBleets.Msg.DataUpdate data) state.SearchBleets

        { state with Home = home; ProfileElem = profileElem; SearchBleets = searchBleets },
        Cmd.batch [
            Cmd.map HomeMsg homeCmd
            Cmd.map ProfileElemMsg profileElemCmd
            Cmd.map SearchBleetsMsg searchBleetCmd
        ]
    | UrlChanged url ->
        match url with
        | [ "bleeter-info" ] -> { state with CurrentUrl = [ "bleeter-info" ] }, Cmd.none
        | [ "home" ] ->
            let home, homeCmd = Home.update Home.Msg.RefreshHome state.Home
            { state with CurrentUrl = url; Home = home }, (Cmd.map HomeMsg homeCmd)
        | [ "search"; (query: string) ] -> 
            let searchBleets, searchCmd = SearchBleets.update (SearchBleets.Msg.Search query) state.SearchBleets
            { state with SearchBleets = searchBleets; CurrentUrl = url }, Cmd.map SearchBleetsMsg searchCmd
        | [ "not-found" ] -> { state with CurrentUrl = url }, Cmd.none
        | [ (handle: string) ] ->
            let profileElem, cmd = ProfileElem.update (ProfileElem.Msg.UrlChanged handle) state.ProfileElem

            { state with
                ProfileElem = profileElem
                CurrentUrl = url
            },
            Cmd.map ProfileElemMsg cmd
        | _ ->
            Router.navigate ("not-found")
            state, Cmd.none
    | AppHeight height -> { state with Height = height }, Cmd.none
    | ProfileElemMsg msg' ->
        let nextProfileElem, profileCmd = ProfileElem.update msg' state.ProfileElem

        if nextProfileElem.DeletedBleet.IsSome then
            { state with
                ProfileElem = nextProfileElem
                DeletedBleet = nextProfileElem.DeletedBleet
            },
            Cmd.map ProfileElemMsg profileCmd
        else
            { state with
                ProfileElem = nextProfileElem
                DeletedBleet = None
            },
            Cmd.map ProfileElemMsg profileCmd
    | HomeMsg msg' ->
        let nextHome, homeCmd = Home.update msg' state.Home

        if nextHome.DeletedBleet.IsSome then
            { state with
                Home = nextHome
                DeletedBleet = nextHome.DeletedBleet
            },
            Cmd.map HomeMsg homeCmd
        else
            { state with Home = nextHome; DeletedBleet = None }, Cmd.map HomeMsg homeCmd
    | SearchBleetsMsg msg' -> 
        let nextSearchBleets, searchBleetsCmd = SearchBleets.update msg' state.SearchBleets
        { state with SearchBleets = nextSearchBleets }, Cmd.map SearchBleetsMsg searchBleetsCmd

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
            tw.``border-l``
            tw.``border-r``
            tw.``h-full``
            tw.``w-full``
        ]
        prop.style [ style.height state.Height ]
        prop.children [
            match state.CurrentUrl with
            | [ "home" ] -> Home.render state.Home (HomeMsg >> dispatch)
            | [ "bleeter-info" ] -> BleeterInfo.page
            | [ "search"; (_: string) ] -> 
                SearchBleets.render state.SearchBleets (SearchBleetsMsg >> dispatch)
            | [ "not-found" ] -> notFoundElem
            | [ (_: string) ] -> ProfileElem.render state.ProfileElem (ProfileElemMsg >> dispatch)
            | _ -> Html.none
        ]
    ]
