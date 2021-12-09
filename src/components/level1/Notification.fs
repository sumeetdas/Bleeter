[<RequireQualifiedAccess>]
module Notification

open Elmish
open Tailwind
open Feliz

type State = { Content: ReactElement option; Display: bool }

type Msg = 
    | Show of ReactElement option
    | Close

let init() = {
    Content = None
    Display = false
}

let closeNotif (state: State) = 
    { state with Display = false; Content = None }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with 
    | Show notifMsgOpt -> 
        match notifMsgOpt with 
        | Some notifMsg -> 
            let delayedClose = 
                async {
                    do! Async.Sleep 5000
                    return Close
                }

            { state with Display = true; Content = Some notifMsg }, Cmd.fromAsync delayedClose
        | None -> closeNotif state, Cmd.none
    | Close -> closeNotif state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [ 
            tw.``fixed`` 
            tw.flex
            tw.``z-50``
            tw.``bottom-4`` 
            tw.``left-1/3`` 
            tw.``w-full`` 
            tw.``h-8`` 
            tw.``pl-12``
            (if state.Display then tw.``block`` else tw.``hidden``)
        ]
        prop.children [
            Html.div [
                prop.onClick (fun _ -> dispatch(Close))
                prop.classes [
                    tw.``px-6``
                    tw.``py-3``
                    tw.``shadow-2xl``
                    tw.``flex``
                    tw.``items-center``
                    tw.``bg-bleeter-blue``
                    tw.``text-white``
                    tw.``w-1/3``
                ]
                prop.children [
                    (
                        match state.Content with 
                        | Some content -> content
                        | None -> Html.none
                    )
                ]
            ]
        ]
    ]
