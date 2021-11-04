module App

open Elmish
open Elmish.React
open Feliz
open Tailwind

// data model
type State =
    { Count: int; Loading: bool }

// events
type Msg = 
    | Increment
    | Decrement
    | IncrementDelayed

// need parentheses for indicating that init is a function
let init() = { Count = 0; Loading = false }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with 
    | Increment -> {state with Loading = false; Count = state.Count + 1}, Cmd.none
    | Decrement -> {state with Count = state.Count - 1}, Cmd.none
    | IncrementDelayed when state.Loading -> state, Cmd.none
    | IncrementDelayed -> 
        let delayedDispatch = async {
                do! Async.Sleep 1000
                return Increment
            }
        {state with Loading = true}, Cmd.fromAsync delayedDispatch

let render (state: State) (dispatch: Msg -> Unit) =
    let content = 
        if state.Loading then Html.h1 "Loading.."
        else Html.h1 state.Count

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``            
        ]
        prop.children [
            SideNav.sideNavTemp
            
            Html.div [
                prop.classes [
                    tw.``flex-grow-2``
                ]
                prop.children [
                    content 
                    Html.button [
                        prop.onClick (fun _ -> dispatch Increment)
                        prop.text "Increment"
                    ]

                    Html.button [
                        prop.onClick (fun _ -> dispatch Decrement)
                        prop.text "Decrement"
                    ]

                    Html.button [
                        prop.onClick (fun _ -> dispatch IncrementDelayed)
                        prop.text "IncrementDelayed"
                    ]
                ]
            ]

            Html.div [
                prop.classes [
                    tw.``flex-grow-1``
                ]
                prop.children [
                    Html.button [
                        prop.onClick (fun _ -> dispatch Increment)
                        prop.text "Increment"
                    ]
                ]
            ]
        ]
        //SideNav.render state dispatch
        
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run