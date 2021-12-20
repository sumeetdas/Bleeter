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
    let image =
        Html.img [
            prop.classes [
                tw.``w-32``
                tw.``h-auto``
            ]
            prop.src "img/ccp.svg"
        ]

    let message1 =
        Html.b [
            prop.classes [
                tw.``mt-4``
                tw.``text-xl``
                tw.``text-center``
                tw.``xl:text-2xl``
            ]
            prop.text "50 social credit points has been deducted from your account."
        ]

    let message2 =
        Html.span [
            prop.classes [
                tw.``mt-3``
                tw.``text-center``
            ]
            prop.text "Contact your local CCP office to revert this decision."
        ]

    let row (elem: ReactElement) =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``w-full``
                tw.``justify-center``
                tw.``mt-2``
            ]
            prop.children [ elem ]
        ]

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``w-full``
            tw.``h-full``
            tw.``bg-red-500``
            tw.``bg-opacity-40``
            tw.``pt-12``
        ]
        prop.children ([ image; message1; message2 ] |> List.map row)
    ]
