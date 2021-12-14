[<RequireQualifiedAccess>]
module Notification

open Elmish
open Tailwind
open Feliz

type State = { Content: ReactElement option; Display: bool }

type Msg =
    | Show of ReactElement option
    | Close

let init () = { Content = None; Display = false }

let closeNotif (state: State) = { state with Display = false; Content = None }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | Show notifMsgOpt ->
        match notifMsgOpt with
        | Some notifMsg ->
            let delayedClose =
                async {
                    do! Async.Sleep 500000
                    return Close
                }

            { state with Display = true; Content = Some notifMsg }, Cmd.fromAsync delayedClose
        | None -> closeNotif state, Cmd.none
    | Close -> closeNotif state, Cmd.none

let msgElem (msg: string) =
    Html.p [
        prop.classes [
            tw.``rounded-full``
            tw.border
            tw.``border-bleeter-blue-hover``
            tw.``leading-9``
            tw.``text-xl``
            tw.``bg-bleeter-blue-hover``
            tw.``text-gray-100``
            tw.``select-none``
            tw.``px-4``
            tw.``bleeter-pointer``
        ]
        prop.text msg
    ]


let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.``fixed``
            tw.``z-50``
            tw.``bottom-4``
            tw.``left-1/3``
            tw.``w-full``
            tw.``h-8``
            tw.``pl-12``
            (if state.Display then tw.flex else tw.hidden)
        ]
        prop.children [
            Html.div [
                prop.onClick (fun _ -> dispatch (Close))
                prop.classes [
                    tw.``px-6``
                    tw.``py-3``
                    tw.``shadow-2xl``
                    tw.flex
                    tw.``items-center``
                    tw.``justify-center``
                    tw.``w-1/3``
                ]
                prop.children [
                    (match state.Content with
                     | Some content -> content
                     | None -> Html.none)
                ]
            ]
        ]
    ]
