[<RequireQualifiedAccess>]
module Trending

open Feliz
open Tailwind

let render =
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
                Html.div [
                    prop.classes [
                        tw.``mt-3``
                        tw.``h-5``
                        tw.``w-5``
                        tw.``p-1``
                        tw.``rounded-full``
                        tw.``cursor-pointer``
                        tw.``hover:bg-green-400``
                    ]
                    prop.children [
                        Bleeter.icon "ant-design:ellipsis-outlined" "12"
                    ]
                ]
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
