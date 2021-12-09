[<RequireQualifiedAccess>]
module Meditation

open Elmish
open Tailwind
open Feliz

type State = { Display: bool }

type Msg = 
    | Display

let init () = { Display = false }

let update (msg: Msg) (state: State) : State * Msg Cmd = 
    match msg with 
    | Display -> { state with Display = true }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) = 
    Html.div [
        Html.text "Yolo"
    ]