[<RequireQualifiedAccess>]
module Trending

open Elmish
open Feliz
open Tailwind

type Msg =
    | TrendOptionMsg of Msg EllipsisOption.Msg
    | ReportTrend

type State = { TrendOption: Msg EllipsisOption.State }

let init () =
    let options: Msg EllipsisOption.Option list = [ { Name = "Report"; Command = Cmd.ofMsg ReportTrend } ]
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
        TrendOption = EllipsisOption.init options size cssClasses offset
    }

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | TrendOptionMsg msg ->
        let trendOption, cmd = EllipsisOption.update msg state.TrendOption
        { state with TrendOption = trendOption }, cmd
    | ReportTrend ->
        printf "Report trend"
        state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let trendList =
        [
            ("Trending in SheepLand", "#SheepCare", "100K Tweets")
            ("Trending in WonderLand", "#AliceRocks", "150K Tweets")
        ]

    let elem (description: string, name: string, numTweets: string) =
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
                            prop.text description
                        ]
                        Html.h2 [
                            prop.classes [
                                tw.``px-4``
                                tw.``w-48``
                                tw.``font-bold``
                            ]
                            prop.text name
                        ]
                        Html.p [
                            prop.classes [
                                tw.``px-4``
                                tw.``w-48``
                                tw.``text-xs``
                                tw.``text-gray-600``
                            ]
                            prop.text numTweets
                        ]
                    ]
                ]
                EllipsisOption.render state.TrendOption (TrendOptionMsg >> dispatch)
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
                prop.text "Trending"
            ]
            Html.div [
                prop.children (trendList |> List.map elem)
            ]
        ]
    ]
