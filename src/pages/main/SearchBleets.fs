[<RequireQualifiedAccess>]
module SearchBleets

open Elmish
open Feliz
open Tailwind

type State =
    {
        Query: string
        Bleets: Bleet list
        BleetListElem: BleetListElem.State
        DeletedBleet: Bleet option
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
    }

type Msg =
    | Search of string
    | DataUpdate of Data.State
    | BleetListElemMsg of BleetListElem.Msg
    | UrlChanged of string list

let getBleets (data: Data.State) =
    match data.Bleets with
    | Resolved (Ok bleets) -> bleets
    | _ -> []

let init (data: Data.State) =
    let bleets = getBleets data

    {
        Query = ""
        Bleets = bleets
        BleetListElem = BleetListElem.init bleets
        DeletedBleet = None
        NotifMsg = None
        ModalMsg = Modal.DoNothing
    }

let filterBleets (state: State, query: string) =
    state.Bleets
    |> List.filter
        (fun bleet ->
            let content = bleet.Content.ToLower()
            content.Contains(query.ToLower()))

let updateBleetListElem (msg: BleetListElem.Msg) (state: State) : State * Cmd<Msg> =
    let nextBleetListElem, bleetListElemCmd = BleetListElem.update msg state.BleetListElem

    { state with
        BleetListElem = nextBleetListElem
        DeletedBleet = nextBleetListElem.DeletedBleet
        NotifMsg = nextBleetListElem.NotifMsg
        ModalMsg = nextBleetListElem.ModalMsg
    },
    Cmd.map BleetListElemMsg bleetListElemCmd

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | UrlChanged url -> updateBleetListElem (BleetListElem.UrlChanged url) state
    | Search query ->
        let filteredBleets = filterBleets (state, query)
        updateBleetListElem (BleetListElem.Msg.DataUpdate filteredBleets) { state with Query = query }
    | DataUpdate data ->
        let bleets = getBleets data
        let nextState = { state with Bleets = bleets }
        let filteredBleets = filterBleets (nextState, state.Query)
        updateBleetListElem (BleetListElem.Msg.DataUpdate filteredBleets) nextState
    | BleetListElemMsg msg' -> updateBleetListElem msg' state

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponents =
        [
            MainLayout.heading (sprintf "Search \"%s\"" state.Query)
            BleetListElem.render state.BleetListElem (BleetListElemMsg >> dispatch)
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
