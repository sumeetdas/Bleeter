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

/// When emitting the message, map to another type
let dupMap (f: 'a -> 'msg) (cmd: Cmd<'a>) : Cmd<'msg> =
    let innerFn = fun d -> f >> d
    let outerFn = fun g -> innerFn >> g
    cmd |> List.map outerFn