module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router
open Tailwind
open Browser
open System

// data model
type State =
    { CurrentUrl: string list; Main: Main.State }

// events
type Msg = 
    | UrlChanged of string list
    | MainMsg of Main.Msg

// need parentheses for indicating that init is a function
let init() = { CurrentUrl = Router.currentUrl(); Main = Main.init() }, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with 
    | UrlChanged url ->
        let main = Main.update (Main.Msg.UrlChanged url) state.Main
        {state with CurrentUrl = url; Main = main}, Cmd.none
    | MainMsg msg' -> 
        let main = Main.update msg' state.Main
        {state with Main = main}, Cmd.none

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
    let page =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``min-h-full``
                tw.``h-full``
            ]
            prop.children [
                Menu.menuHtml
                
                (Main.render state.Main)

                Html.div [
                    prop.classes [
                        tw.``flex-grow-1``
                    ]
                    prop.children [
                        searchBox
                        trending
                    ]
                ]
            ]        
        ]

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)
        router.children page
    ]

let appHeight initial =
    let sub dispatch = window.addEventListener("load", 
        fun _ -> 
            let scrollHeight = (document.getElementById "elmish-app").scrollHeight |> int
            let windowHeight = window.innerHeight |> int
            let finalHeight = if scrollHeight > windowHeight then scrollHeight else windowHeight
            (Main.Msg.AppHeight >> MainMsg >> dispatch) finalHeight
    )
    Cmd.ofSub sub

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription appHeight
|> Program.run