module App

open Elmish
open Elmish.React
open Feliz

// data model
type State =
    { Count: int }

// events
type Msg = 
    | Increment
    | Decrement

// need parentheses for indicating that init is a function
let init() = { Count = 0 }

let update (msg: Msg) (state: State): State =
    match msg with 
    | Increment -> {state with Count = state.Count + 1}
    | Decrement -> {state with Count = state.Count - 1}

let render (state: State) (dispatch: Msg -> Unit) =
    Html.div [
        Html.button [
            prop.onClick (fun _ -> dispatch Increment)
            prop.text "Increment"
        ]

        Html.button [
            prop.onClick (fun _ -> dispatch Decrement)
            prop.text "Decrement"
        ]

        Html.h1 state.Count
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run