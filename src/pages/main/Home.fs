[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State =
    {
        BleetListElem: BleetListElem.State
        DeletedBleet: Bleet option
        HeightUpdated: bool
    }

type Msg =
    | RefreshHome
    | DataUpdate of Data.State
    | BleetListElemMsg of BleetListElem.Msg

let getBleets (data: Data.State) =
    match data.Bleets with
    | Resolved (Ok bleets) -> bleets
    | _ -> []

let init (data: Data.State) =
    let bleets = getBleets data

    {
        BleetListElem = BleetListElem.init bleets
        DeletedBleet = None
        HeightUpdated = false
    }

let updateBleetListElem (msg: BleetListElem.Msg) (state: State) : State * Cmd<Msg> =
    let nextBleetListElem, bleetListElemCmd = BleetListElem.update msg state.BleetListElem

    { state with
        BleetListElem = nextBleetListElem
        DeletedBleet = nextBleetListElem.DeletedBleet
        HeightUpdated = nextBleetListElem.HeightUpdated
    },
    Cmd.map BleetListElemMsg bleetListElemCmd

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | RefreshHome -> updateBleetListElem (BleetListElem.Msg.RefreshBleetList) state
    | DataUpdate data ->
        let bleets = getBleets data
        updateBleetListElem (BleetListElem.Msg.DataUpdate bleets) state
    | BleetListElemMsg msg' -> updateBleetListElem msg' state

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponents =
        [
            MainLayout.heading "Home"
            BleetListElem.render state.BleetListElem (BleetListElemMsg >> dispatch)
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
