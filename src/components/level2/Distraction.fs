[<RequireQualifiedAccess>]
module Distraction

open Elmish
open Feliz
open Tailwind

type Msg =
    | DistractionOptionMsg of Msg EllipsisOption.Msg
    | ReportDistraction
    | DataUpdate of Data.State

type State =
    {
        DistractionOption: Msg EllipsisOption.State
        Data: Data.State
    }

let init (data: Data.State) =
    let options: Msg EllipsisOption.Option list =
        [
            { Name = "Report"; Command = Cmd.ofMsg ReportDistraction }
        ]

    let size = 12

    let cssClasses =
        [
            tw.``mt-3``
            tw.``rounded-full``
            tw.``cursor-pointer``
            tw.``hover:bg-green-400``
        ]

    let offset = { X = -100.0; Y = 15.0 }

    {
        DistractionOption = EllipsisOption.init options size cssClasses offset
        Data = data
    }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | DataUpdate data -> { state with Data = data }, Cmd.none
    | DistractionOptionMsg msg ->
        let distractionOption, cmd = EllipsisOption.update msg state.DistractionOption
        { state with DistractionOption = distractionOption }, cmd
    | ReportDistraction ->
        printf "Report distraction"
        state, Cmd.none

let distractionList (state: State) : Distraction list =
    match state.Data.Distractions with
    | Resolved (Ok distractions) -> distractions |> List.truncate 4
    | _ -> []

let render (state: State) (dispatch: Msg -> unit) =
    let elem (distraction: Distraction) =
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
                            prop.href (sprintf "#/distractions/%s" (distraction.Hashtag.Substring 1))
                            prop.children [
                                Html.h2 [
                                    prop.classes [
                                        tw.``px-4``
                                        tw.``w-48``
                                        tw.``font-bold``
                                    ]
                                    prop.text distraction.Hashtag
                                ]
                            ]
                        ]
                        // will be implemented later
                        // Html.p [
                        //     prop.classes [
                        //         tw.``px-4``
                        //         tw.``w-48``
                        //         tw.``text-xs``
                        //         tw.``text-gray-600``
                        //     ]
                        //     prop.text numBleets
                        // ]
                        ]
                ]
                EllipsisOption.render state.DistractionOption (DistractionOptionMsg >> dispatch)
            ]
        ]

    Html.div [
        prop.classes [
            tw.``max-w-sm``
            tw.``rounded-lg``
            tw.``bg-bleet-dim``
            tw.``overflow-hidden``
            tw.``shadow-lg``
            tw.``m-4``
        ]
        prop.children [
            Html.h2 [
                prop.classes [
                    tw.``m-4``
                    tw.``px-4``
                    tw.``py-2``
                    tw.``text-xl``
                    tw.``w-48``
                    tw.``font-semibold``
                ]
                prop.text "Distractions"
            ]
            Html.div [
                prop.children ((distractionList state) |> List.map elem)
            ]
        ]
    ]
