[<RequireQualifiedAccess>]
module Main

open Feliz
open Feliz.Router
open Tailwind

type State =
    { CurrentUrl: string list
      Height: int
      BleeterProfile: BleeterProfile.State }

type Msg =
    | UrlChanged of string list
    | AppHeight of int
    | BleeterProfileMsg of BleeterProfile.Msg

let init () =
    { CurrentUrl = Router.currentUrl ()
      Height = 0
      BleeterProfile = BleeterProfile.init () }

let update (msg: Msg) (state: State) : State =
    match msg with
    | UrlChanged url -> { state with CurrentUrl = url }
    | AppHeight height -> { state with Height = height }
    | BleeterProfileMsg msg' ->
        let bleeterProfile = BleeterProfile.update msg' state.BleeterProfile
        {state with BleeterProfile = bleeterProfile}

let render (state: State) (dispatch:Msg->unit) =
    Html.div [ prop.classes [ tw.flex
                              tw.``flex-grow-1``
                              tw.``max-w-screen-sm``
                              tw.``border-l``
                              tw.``border-r``
                              tw.``h-full``
                              tw.``w-full`` ]
               prop.style [ style.height state.Height ]
               prop.children [ match state.CurrentUrl with
                               | [ "home" ] -> Home.render
                               | [ "profile" ] -> BleeterProfile.render state.BleeterProfile (BleeterProfileMsg >> dispatch)
                               | [ "bleeter-info" ] -> BleeterInfo.page
                               | _ -> BleeterProfile.render state.BleeterProfile (BleeterProfileMsg >> dispatch) ] ]
