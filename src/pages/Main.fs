[<RequireQualifiedAccess>]
module Main

open Elmish
open Feliz
open Feliz.Router
open Tailwind

type State =
    {
        CurrentUrl: string list
        Height: int
        ProfileElem: ProfileElem.State
        Home: Home.State
    }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | ProfileElemMsg of ProfileElem.Msg
    | HomeMsg of Home.Msg

let init (currentUrl: string list) : State * Msg Cmd =
    {
        CurrentUrl = currentUrl
        Height = 0
        ProfileElem = ProfileElem.init ()
        Home = Home.init ()
    },
    Cmd.ofMsg (UrlChanged currentUrl)

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | UrlChanged url ->
        match url with
        | [ "bleeter-info" ] -> { state with CurrentUrl = [ "bleeter-info" ] }, Cmd.none
        | [ "home" ] ->
            let home, homeCmd = Home.update (Home.Msg.LoadBleets Started) state.Home
            { state with CurrentUrl = [ "home" ]; Home = home }, (Cmd.map HomeMsg homeCmd)
        | [ (handle: string) ] ->
            let profileElem, cmd = ProfileElem.update (ProfileElem.Msg.UrlChanged handle) state.ProfileElem

            { state with
                ProfileElem = profileElem
                CurrentUrl = [ handle ]
            },
            Cmd.map ProfileElemMsg cmd
        | _ -> state, Cmd.none
    | AppHeight height -> { state with Height = height }, Cmd.none
    | ProfileElemMsg msg' ->
        let profileElem, cmd = ProfileElem.update msg' state.ProfileElem
        { state with ProfileElem = profileElem }, (Cmd.map ProfileElemMsg cmd)
    | HomeMsg msg' ->
        let newHome, cmd = Home.update msg' state.Home
        { state with Home = newHome }, (Cmd.map HomeMsg cmd)

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
            | [ (_: string) ] -> ProfileElem.render state.ProfileElem (ProfileElemMsg >> dispatch)
            | _ -> Html.none
        ]
    ]
