[<RequireQualifiedAccess>]
module DistractionBleets

open Elmish
open Feliz
open Tailwind

type State =
    {
        HashTag: string
        Bleets: Bleet list
        BleetListElem: BleetListElem.State
        DeletedBleet: Bleet option
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
    }

type Msg =
    | LoadTaggedBleets of string
    | DataUpdate of Data.State
    | BleetListElemMsg of BleetListElem.Msg

let getBleets (data: Data.State) =
    match data.Bleets with
    | Resolved (Ok bleets) -> bleets
    | _ -> []

let init (data: Data.State) =
    let bleets = getBleets data

    {
        HashTag = ""
        Bleets = []
        BleetListElem = BleetListElem.init bleets
        DeletedBleet = None
        NotifMsg = None
        ModalMsg = Modal.DoNothing
    }

let filterBleets (state: State, tag: string) =
    state.Bleets
    |> List.filter
        (fun bleet ->
            let content = bleet.Content.ToLower()
            content.Contains(sprintf "#%s" (tag.ToLower())))

let updateBleetListElem (msg: BleetListElem.Msg) (state: State) : State * Cmd<Msg> =
    let nextBleetListElem, bleetListElemCmd = BleetListElem.update msg state.BleetListElem

    let nextBleetListElem, _ =
        BleetListElem.update
            (BleetListElem.UrlChanged [
                "tags"
                state.HashTag
             ])
            nextBleetListElem

    { state with
        BleetListElem = nextBleetListElem
        DeletedBleet = nextBleetListElem.DeletedBleet
        NotifMsg = nextBleetListElem.NotifMsg
        ModalMsg = nextBleetListElem.ModalMsg
    },
    Cmd.map BleetListElemMsg bleetListElemCmd

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | LoadTaggedBleets tag ->
        let bleets = filterBleets (state, tag)
        updateBleetListElem (BleetListElem.Msg.DataUpdate bleets) { state with HashTag = tag }
    | DataUpdate data ->
        let bleets = getBleets data
        let nextState = { state with Bleets = bleets }
        let filteredBleets = filterBleets (nextState, state.HashTag)
        updateBleetListElem (BleetListElem.Msg.DataUpdate filteredBleets) nextState
    | BleetListElemMsg msg' -> updateBleetListElem msg' state

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponents =
        [
            MainLayout.heading (sprintf "#%s Bleets" state.HashTag)
            BleetListElem.render state.BleetListElem (BleetListElemMsg >> dispatch)
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
