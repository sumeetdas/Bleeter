module App

open Elmish
open Elmish.React
open Feliz
open Tailwind

// data model
type State =
    { Count: int; Loading: bool }

// events
type Msg = 
    | Increment
    | Decrement
    | IncrementDelayed

// need parentheses for indicating that init is a function
let init() = { Count = 0; Loading = false }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with 
    | Increment -> {state with Loading = false; Count = state.Count + 1}, Cmd.none
    | Decrement -> {state with Count = state.Count - 1}, Cmd.none
    | IncrementDelayed when state.Loading -> state, Cmd.none
    | IncrementDelayed -> 
        let delayedDispatch = async {
                do! Async.Sleep 1000
                return Increment
            }
        {state with Loading = true}, Cmd.fromAsync delayedDispatch

let searchBox = 
    Html.div [ 
        prop.classes [
            tw.``bg-bleet-dim``
            tw.``text-gray-500``
            tw.``w-96``
            tw.``h-10``
            tw.``m-4``
            tw.``focus-within:ring``
            tw.``focus-within:border-blue-300``
            tw.``rounded-full``
            tw.``flex``
        ]
        prop.children [
            Html.button [
                prop.classes [
                    tw.``mx-3``
                ]
                prop.children [
                    Bleeter.icon "ant-design:search-outlined" "24"
                ]
            ]
            Html.input [
                prop.classes [
                    tw.``bg-bleet-dim``
                    tw.``h-10``
                    tw.``w-72``
                    tw.``text-sm``
                    tw.``border-0``
                    tw.``focus:outline-none``
                ]
                prop.placeholder "Search Bleeter"
            ]
            Html.button [ 
                prop.classes [ 
                    tw.``text-green-600`` 
                    tw.``mx-3``
                ]
                prop.children [
                    Bleeter.icon "gridicons:cross-circle" "24"
                ]
            ]
        ]
    ]

let trending = 
    let trendList = [
        ("Trending in SheepLand", "#SheepCare", "100K Tweets")
        ("Trending in WonderLand", "#AliceRocks", "150K Tweets")
    ]

    let elem (description:string, name:string, numTweets:string) = 
        Html.div [ 
            prop.classes [
                tw.``flex``
                tw.``m-4``
            ]
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

let render (state: State) (dispatch: Msg -> Unit) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``
        ]
        prop.children [
            Menu.menuHtml
            
            Html.div [
                prop.classes [
                    tw.``flex-grow-1``
                    tw.``max-w-screen-sm``
                ]
                prop.children [
                    Main.mainElem
                ]
            ]

            Html.div [
                prop.classes [
                    tw.``flex-grow-1``
                ]
                prop.children [
                    searchBox
                    trending
                    Html.a [
                        prop.children [
                            Html.span [
                                prop.text "About Bleeter"
                            ]
                        ]
                    ]
                    Html.br []
                ]
            ]
        ]        
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run