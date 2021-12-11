[<RequireQualifiedAccess>]
module DistractionElem

open Elmish
open Feliz
open Tailwind

type Msg =
    | DataUpdate of Distraction
    | DistractionOptionMsg of Msg EllipsisOption.Msg
    | ReportDistraction

type State =
    {
        DistractionOption: Msg EllipsisOption.State
        ModalMsg: Modal.Msg
        NotifMsg: ReactElement option
        ReportCount: int option
        Distraction: Distraction 
        PreviousUrl: string list
    }

let init (previousUrl: string list) (distraction: Distraction) =
    let options: Msg EllipsisOption.Option list =
        [
            { Name = "Report"; Command = Cmd.ofMsg ReportDistraction }
        ]

    let size = 12

    let cssClasses =
        [
            tw.``mt-3``
            tw.``rounded-full``
            tw.``bleeter-pointer``
            tw.``hover:bg-green-500``
            tw.``hover:text-white``
            tw.``text-xl``
        ]

    let offset = { X = -120.0; Y = 0.0 }

    {
        DistractionOption = EllipsisOption.init options size cssClasses offset
        ModalMsg = Modal.DoNothing
        NotifMsg = None
        ReportCount = None
        Distraction = distraction
        PreviousUrl = previousUrl
    }

let report (state: State) (first: string, second: string, modalMsg: Modal.Msg) : State * Msg Cmd = 
    let nextState =
        match state.ReportCount with
        | None ->
            { state with
                NotifMsg = Some(Notification.msgElem first)
                ReportCount = Some 1
            }
        | Some 1 ->
            { state with
                NotifMsg = Some(Notification.msgElem second)
                ReportCount =
                    state.ReportCount
                    |> Option.bind (fun count -> Some(count + 1))
            }
        | Some _ ->
            { state with
                ModalMsg = modalMsg
            }

    let distractionOption, distractionCmd = EllipsisOption.update (EllipsisOption.Msg.Close) nextState.DistractionOption
    { nextState with DistractionOption = distractionOption }, distractionCmd

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    let state = { state with ModalMsg = Modal.DoNothing; NotifMsg = None }
    match msg with
    | DataUpdate distraction -> 
        { state with Distraction = distraction }, Cmd.none
    | DistractionOptionMsg msg ->
        let distractionOption, cmd = EllipsisOption.update msg state.DistractionOption
        { state with DistractionOption = distractionOption }, cmd
    | ReportDistraction -> 
        if state.Distraction.Hashtag.Contains("Xiowei")
        then 
            let first = "Operation not permitted!"
            let second = "OPERATION NOT PERMITTED!!!"
            let modalMsg = Modal.ShowMeditation state.PreviousUrl
            report state (first, second, modalMsg)  
        else 
            let first = sprintf "We've reported %s to Bleeter police." state.Distraction.Hashtag
            let second = "We get it. You are pissed."
            let modalMsg = Modal.ShowMeditation state.PreviousUrl
            report state (first, second, modalMsg)

let render (state: State) (dispatch: Msg -> unit) =
    let distraction = state.Distraction

    Html.div [
        prop.classes [ tw.flex; tw.``m-4`` ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.``flex-grow-2``
                    tw.``mt-3``
                    tw.``mb-3``
                ]
                prop.children [
                    Html.p [
                        prop.classes [
                            tw.``px-4``
                            tw.``w-48``
                            tw.``text-xs``
                            tw.``text-gray-600``
                        ]
                        prop.text distraction.Category
                    ]
                    Html.a [
                        prop.href (sprintf "#/tags/%s" (distraction.Hashtag.Substring 1))
                        prop.children [
                            Html.h2 [
                                prop.classes [
                                    tw.``px-4``
                                    tw.``w-48``
                                    tw.``font-bold``
                                    tw.``text-green-600``
                                ]
                                prop.text distraction.Hashtag
                            ]
                        ]
                    ]
                ]
            ]
            EllipsisOption.render state.DistractionOption (DistractionOptionMsg >> dispatch)
        ]
    ]