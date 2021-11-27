[<RequireQualifiedAccess>]
module Distraction

open Elmish
open Feliz
open Tailwind

type Msg =
    | DistractionOptionMsg of Msg EllipsisOption.Msg
    | ReportDistraction

type State = { DistractionOption: Msg EllipsisOption.State }

let init () =
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
    }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | DistractionOptionMsg msg ->
        let distractionOption, cmd = EllipsisOption.update msg state.DistractionOption
        { state with DistractionOption = distractionOption }, cmd
    | ReportDistraction ->
        printf "Report distraction"
        state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let distractionList: Distraction list = []

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
                        Html.h2 [
                            prop.classes [
                                tw.``px-4``
                                tw.``w-48``
                                tw.``font-bold``
                            ]
                            prop.text distraction.HashTag
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
                prop.text "Distraction"
            ]
            Html.div [
                prop.children (distractionList |> List.map elem)
            ]
        ]
    ]
