[<RequireQualifiedAccess>]
module MobileMenu

open Feliz
open Tailwind

type State = { Display: bool }

type Msg = 
    | Display
    | Close

let init () = { Display = false }

let update (msg: Msg) (state: State) : State = 
    match msg with 
    | Display -> { state with Display = true }
    | Close -> { state with Display = false }

let render (state: State) (dispatch: Msg -> unit) = 
    Html.div [
        
    ]