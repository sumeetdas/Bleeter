[<RequireQualifiedAccess>]
module SingleBleetPage

open Elmish
open Feliz
open Feliz.Router
open Tailwind

type State =
    {
        Data: Data.State
        BleetElem: BleetElem.State option
        Handle: string
        BleetId: int
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
        DeleteBleet: Bleet option
        PreviousUrl: string list
    }

type Msg =
    | LoadBleet of string * int
    | BleetElemMsg of BleetElem.Msg
    | DataUpdate of Data.State
    | PreviousUrlUpdate of string list

let init (data: Data.State) : State =
    {
        Data = data
        BleetElem = None
        Handle = ""
        BleetId = 0
        NotifMsg = None
        ModalMsg = Modal.DoNothing
        DeleteBleet = None
        PreviousUrl = []
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
    | PreviousUrlUpdate url -> { state with PreviousUrl = url }, Cmd.none
    | LoadBleet (handle: string, bleetId: int) -> updateData { state with Handle = handle; BleetId = bleetId }
    | BleetElemMsg msg' ->
        match state.BleetElem with
        | Some bleetElem ->
            let nextBleetElem, bleetElemCmd = BleetElem.update msg' bleetElem

            if nextBleetElem.IsDeleted then
                Router.navigate ("#/" + (state.PreviousUrl |> String.concat "/"))
            else
                1 |> ignore

            { state with
                BleetElem = Some nextBleetElem
                NotifMsg = nextBleetElem.NotifMsg
                ModalMsg = nextBleetElem.ModalMsg
                DeleteBleet =
                    if nextBleetElem.IsDeleted then
                        Some nextBleetElem.Bleet
                    else
                        None
            },
            Cmd.map BleetElemMsg bleetElemCmd
        | None -> state, Cmd.none
    | DataUpdate data -> updateData { state with Data = data }

let render (state: State) (dispatch: Msg -> unit) =
    let coreComponents =
        [
            MainLayout.heading "Bleet and Replies"
            (match state.BleetElem with
             | Some bleetElem -> BleetElem.render bleetElem (BleetElemMsg >> dispatch)
             | None -> Html.none)
            (match state.BleetElem with
             | Some bleetElem ->
                 let bleet = bleetElem.Bleet

                 match (bleet.RepliesType, bleet.Replies) with
                 | Some repliesType, numReplies when numReplies > 0 ->
                     Html.div [
                         prop.classes [
                             tw.flex
                             tw.``flex-col``
                             tw.``w-full``
                             tw.``bg-green-500``
                             tw.``justify-center``
                             tw.``mx-auto``
                             tw.``text-xl``
                             tw.``text-gray-100``
                             tw.``h-24``
                             tw.``sm:h-20``
                         ]
                         prop.children [
                             Html.p [
                                 prop.classes [ tw.``text-center`` ]
                                 prop.text (repliesType |> RepliesType.getReasonToHideReplies)
                             ]
                         ]
                     ]
                 | _ -> Html.none
             | None -> Html.none)
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
