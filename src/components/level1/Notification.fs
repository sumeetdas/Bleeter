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
            printf "show notif %A" notifMsg
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
            tw.flex
            tw.``justify-center``
            tw.``z-50``
            (if state.Display then tw.``block`` else tw.``hidden``)
        ]
        prop.children [
            Html.div [
                prop.onClick (fun _ -> dispatch(Close))
                prop.classes [
                    tw.``w-full``
                    tw.``px-6``
                    tw.``py-3``
                    tw.``shadow-2xl``
                    tw.``flex``
                    tw.``items-center``
                    tw.``bg-blue-600``
                    tw.``text-white``
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
