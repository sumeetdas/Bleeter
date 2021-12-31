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
                tw.``sm:w-16``
                tw.``md:w-20``
                tw.``lg:w-24``
                tw.``xl:w-28``
                tw.``h-auto``
            ]
            prop.src "/Bleeter/img/ccp.svg"
        ]

    let message1 =
        Html.b [
            prop.classes [
                tw.``mt-4``
                tw.``text-xl``
                tw.``text-center``
                tw.``xl:text-2xl``
                tw.``text-yellow-300``
            ]
            prop.text "50 social credit points has been deducted from your account."
        ]

    let message2 =
        Html.span [
            prop.classes [
                tw.``mt-3``
                tw.``text-center``
                tw.``text-yellow-300``
                tw.``xl:text-xl``
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
            tw.``bg-red-600``
            tw.``pt-12``
        ]
        prop.children ([ image; message1; message2 ] |> List.map row)
    ]
