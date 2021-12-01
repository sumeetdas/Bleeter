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
    }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | ProfileElemMsg of ProfileElem.Msg
    | HomeMsg of Home.Msg
    | DataUpdate of Data.State

let init (currentUrl: string list) (data: Data.State) : State * Msg Cmd =
    {
        CurrentUrl = currentUrl
        Height = 0
        ProfileElem = ProfileElem.init data
        Home = Home.init data
        DeletedBleet = None
    },
    Cmd.none

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DataUpdate data ->
        let home, homeCmd = Home.update (Home.Msg.DataUpdate data) state.Home
        let profileElem, profileElemCmd = ProfileElem.update (ProfileElem.Msg.DataUpdate data) state.ProfileElem

        { state with Home = home; ProfileElem = profileElem },
        Cmd.batch [
            Cmd.map HomeMsg homeCmd
            Cmd.map ProfileElemMsg profileElemCmd
        ]
    | UrlChanged url ->
        match url with
        | [ "bleeter-info" ] -> { state with CurrentUrl = [ "bleeter-info" ] }, Cmd.none
        | [ "home" ] ->
            let home, homeCmd = Home.update Home.Msg.ClearHomeState state.Home
            { state with CurrentUrl = [ "home" ]; Home = home }, (Cmd.map HomeMsg homeCmd)
        | [ "not-found" ] -> 
            { state with CurrentUrl = [ "not-found" ] }, Cmd.none
        | [ (handle: string) ] ->
            let profileElem, cmd = ProfileElem.update (ProfileElem.Msg.UrlChanged handle) state.ProfileElem

            { state with
                ProfileElem = profileElem
                CurrentUrl = [ handle ]
            },
            Cmd.map ProfileElemMsg cmd
        | _ -> 
            Router.navigate( "not-found" )
            state, Cmd.none
    | AppHeight height -> { state with Height = height }, Cmd.none
    | ProfileElemMsg msg' ->
        let profileElem, cmd = ProfileElem.update msg' state.ProfileElem
        { state with ProfileElem = profileElem }, (Cmd.map ProfileElemMsg cmd)
    | HomeMsg msg' ->
        let nextHome, homeCmd = Home.update msg' state.Home

        if nextHome.DeletedBleet.IsSome 
        then { state with Home = nextHome; DeletedBleet = nextHome.DeletedBleet }, Cmd.map HomeMsg homeCmd
        else { state with Home = nextHome }, Cmd.map HomeMsg homeCmd

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
            | [ "not-found" ] -> notFoundElem
            | [ (_: string) ] -> ProfileElem.render state.ProfileElem (ProfileElemMsg >> dispatch)
            | _ -> Html.none
        ]
    ]
