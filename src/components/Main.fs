[<RequireQualifiedAccess>]
module Main

open Feliz
open Feliz.Router
open Tailwind

type State = { CurrentUrl: string list; Height: int }

type Msg = 
    | UrlChanged of string list
    | AppHeight of int

// let init() = { CurrentUrl = Router.currentUrl(); Height = window.innerHeight |> int }
let init() = { CurrentUrl = Router.currentUrl(); Height = 0 }
// let height = document.readyState (document.getElementById "elmish-app").scrollHeight |> int
    

let update (msg:Msg) (state:State) : State =
    match msg with
    | UrlChanged url -> { state with CurrentUrl = url }
    | AppHeight height -> 
        { state with Height = height }

let render (state:State) = 
    Html.div [
        prop.classes [
            tw.``flex``
            tw.``flex-grow-1``
            tw.``max-w-screen-sm``
            tw.``border-l``
            tw.``border-r``
            tw.``h-full``
        ]
        prop.style [
            style.height state.Height
        ]
        prop.children [
            match state.CurrentUrl with 
            | [ "home" ] -> Home.render
            | [ "profile" ] -> Profile.page
            | [ "bleeter-info" ] -> BleeterInfo.page
            | _ -> Profile.page
        ]
    ]
    
    