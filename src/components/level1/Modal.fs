[<RequireQualifiedAccess>]
module Modal

open Elmish
open Tailwind
open Feliz

type State = { Content: ReactElement option ; Display: bool }

type Msg = 
    | Show of ReactElement option 
    | Close 

let init() = {
    Content = None
    Display = false
}

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with 
    | Show modalMsg -> state, Cmd.none
    | Close -> state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [

    ]
