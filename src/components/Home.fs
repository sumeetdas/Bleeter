[<RequireQualifiedAccess>]
module Home

open Cmd
open Elmish
open Elmish.React
open Feliz
open Tailwind

type State = { Count : int }

type Msg =
    | SomeM

let initState() = { Count = 0 }

let init() = initState(), Cmd.none

let update (msg:Msg) (state:State) : State * Cmd<Msg> =
    state, Cmd.none

let render = 
    Html.div [
        prop.classes [ 
            tw.``ml-3``
        ]
        prop.children [
            Html.text "Hello Home!!!!!!!!!!!!"
        ]
        
    ]