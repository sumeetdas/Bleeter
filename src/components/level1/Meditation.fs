[<RequireQualifiedAccess>]
module Meditation

open Elmish
open Tailwind
open Feliz

type State = { Display: bool }

type Msg = | Display

let init () = { Display = false }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | Display -> { state with Display = true }, Cmd.none

let render (_: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``w-full``
            tw.``h-auto``
        ]
        prop.children [
            Bleeter.ytEmbed "https://www.youtube.com/watch?v=ZToicYcHIOU"
        ]
    ]
