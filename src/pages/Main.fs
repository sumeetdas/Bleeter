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
        BleeterProfile: BleeterProfile.State
        Home: Home.State
    }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | BleeterProfileMsg of BleeterProfile.Msg
    | HomeMsg of Home.Msg

let init (currentUrl: string list): State * Msg Cmd =
    {
        CurrentUrl = currentUrl
        Height = 0
        BleeterProfile = BleeterProfile.init ()
        Home = Home.init ()
    }, Cmd.ofMsg (UrlChanged currentUrl)

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | UrlChanged url -> 
        match url with
        | [ "bleeter-info" ] -> {state with CurrentUrl = [ "bleeter-info" ]}, Cmd.none
        | [ "home" ] -> {state with CurrentUrl = [ "home" ]}, Cmd.none
        | [ (handle:string) ] -> 
            let bleeterProfile, cmd = BleeterProfile.update (BleeterProfile.Msg.UrlChanged handle) state.BleeterProfile
            {state with BleeterProfile = bleeterProfile; CurrentUrl = [ handle ]}, Cmd.map BleeterProfileMsg cmd
        | _ -> 
            state, Cmd.none
    | AppHeight height -> { state with Height = height }, Cmd.none
    | BleeterProfileMsg msg' ->
        let bleeterProfile, cmd = BleeterProfile.update msg' state.BleeterProfile
        { state with BleeterProfile = bleeterProfile }, (Cmd.map BleeterProfileMsg cmd)
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
            | [ (_:string) ] -> BleeterProfile.render state.BleeterProfile (BleeterProfileMsg >> dispatch)
            | _ -> Html.none
        ]
    ]
