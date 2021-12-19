[<RequireQualifiedAccess>]
module CCP

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
            tw.``sm:flex-row``
            tw.``w-full``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                    tw.``justify-center``
                ]
                prop.children [
                    Html.img [
                        prop.classes [
                            tw.``w-32``
                            tw.``h-auto``
                        ]
                        prop.src "img/ccp.svg"
                    ]
                ]
            ]
            Html.p [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                    tw.``mt-2``
                    tw.``sm:ml-2``
                ]
                prop.children [
                    Html.b [
                        prop.text "50 social credit points has been deducted from your account."
                    ]
                    Html.span [
                        prop.classes [ tw.``mt-2`` ]
                        prop.text "Contact your local CCP office to revert this decision."
                    ]
                ]
            ]
        ]
    ]
