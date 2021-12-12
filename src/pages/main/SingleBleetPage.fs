[<RequireQualifiedAccess>]
module SingleBleetPage

open Elmish
open Feliz

type State =
    {
        Data: Data.State
        BleetElem: BleetElem.State option
        Handle: string
        BleetId: int
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
    }

type Msg =
    | LoadBleet of string * int
    | BleetElemMsg of BleetElem.Msg
    | DataUpdate of Data.State

let init (data: Data.State) : State =
    {
        Data = data
        BleetElem = None
        Handle = ""
        BleetId = 0
        NotifMsg = None
        ModalMsg = Modal.DoNothing
    }

let updateData (state: State) : State * Msg Cmd =
    let previousUrl (bleet: Bleet) = [ bleet.Handle; "bleets"; (bleet.Id |> string) ]

    let bleetElem =
        match state.Data.Bleets with
        | Resolved (Ok bleets) -> bleets
        | _ -> []
        |> List.tryFind (fun bleet -> bleet.Handle = state.Handle && bleet.Id = state.BleetId)
        |> Option.bind (fun bleet -> Some(BleetElem.init (previousUrl bleet) bleet))

    { state with BleetElem = bleetElem }, Cmd.none

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | LoadBleet (handle: string, bleetId: int) -> updateData { state with Handle = handle; BleetId = bleetId }
    | BleetElemMsg msg' ->
        match state.BleetElem with
        | Some bleetElem ->
            let nextBleetElem, bleetElemCmd = BleetElem.update msg' bleetElem

            { state with
                BleetElem = Some nextBleetElem
                NotifMsg = nextBleetElem.NotifMsg
                ModalMsg = nextBleetElem.ModalMsg
            },
            Cmd.map BleetElemMsg bleetElemCmd
        | None -> state, Cmd.none
    | DataUpdate data -> updateData { state with Data = data }

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponents =
        [
            match state.BleetElem with
            | Some bleetElem -> BleetElem.render bleetElem (BleetElemMsg >> dispatch)
            | None -> Html.none
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
