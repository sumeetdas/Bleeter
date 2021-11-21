[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State = { Count: int }

type Msg = | SomeM

let initState () = { Count = 0 }

let init () = initState (), Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> = state, Cmd.none

let render =
    Html.div [
        prop.classes [
            tw.``ml-3``
            tw.flex
            tw.``flex-col``
        ]
        prop.children [
            Html.div [
                prop.classes [ tw.flex; tw.``h-32`` ]
                prop.text "Home"
            ]
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                ]
                prop.children bleets
            ]
        ]
    ]
