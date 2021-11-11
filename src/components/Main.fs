[<RequireQualifiedAccess>]
module Main

open Elmish
open Feliz
open Feliz.Router
open Tailwind

type State = { CurrentUrl: string list }

type Msg = | UrlChanged of string list

let init() = { CurrentUrl = Router.currentUrl() }

let update (msg:Msg) (state:State) : State =
    match msg with
    | UrlChanged url -> { CurrentUrl = url }

let render (state:State) = 
    printf "%A" state.CurrentUrl

    match state.CurrentUrl with 
    | [ "home" ] -> Home.render
    | [ "profile" ] -> Profile.page
    | [ "bleeter-info" ] -> BleeterInfo.page
    | _ -> Profile.page
    