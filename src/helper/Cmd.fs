module Cmd 

open Elmish

let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
        let delayedDispatch = async {
            let! msg = operation
            dispatch msg
        }

        Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd