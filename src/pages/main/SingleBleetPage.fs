[<RequireQualifiedAccess>]
module SingleBleetPage

open Elmish
open Feliz
open Tailwind

type State = { BleetElem: BleetElem.State }

type Msg = BleetElemMsg of BleetElem.Msg

let init (bleet: Bleet) : State = { BleetElem = BleetElem.init bleet }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | BleetElemMsg msg' ->
        let nextBleetElem, bleetElemCmd = BleetElem.update msg' state.BleetElem
        { state with BleetElem = nextBleetElem }, Cmd.map BleetElemMsg bleetElemCmd

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
        ]
        prop.children [
            BleetElem.render state.BleetElem (BleetElemMsg >> dispatch)
        ]
    ]
