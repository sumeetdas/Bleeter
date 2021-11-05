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

let main = 
    Html.div [ 
        Html.div [ 
            Html.img [
                prop.classes [
                    tw.``w-full``
                    tw.``bg-cover``
                ]
                prop.src "/bleeter_banner.jpg"
            ]
            Html.img [
                prop.classes [
                    tw.``h-24``
                    tw.``w-24``
                    tw.``rounded-full``
                    tw.``border-4``
                    tw.``border-indigo-400``
                ]
                prop.src "/bleeter_profile_pic.png"
            ]
            Html.span [ 
                prop.text "Bleeter"
            ]
            Html.span [ 
                prop.text "@bleeter"
            ]
            Html.span [ 
                prop.text "Hill"
            ]
            Html.span [ 
                prop.text "sumeetdas.me/bleeter"
            ]
            Html.span [
                prop.text ("30" + " Following") 
            ]
            Html.span [
                prop.text ("24" + " Followers")
            ]
        ]
    ]

let trending = 
    let trendList = [
        ("Trending in SheepLand", "#SheepCare", "100K Tweets")
        ("Trending in WonderLand", "#AliceRocks", "150K Tweets")
    ]

    let elem (description:string, name:string, numTweets:string) = Html.div [ 
        Html.span [
            prop.text description
        ]
        Html.span [
            prop.text name
        ]
        Html.span [
            prop.text numTweets
        ]
    ]

    Html.div [ 
        prop.children (trendList |> List.map elem)
    ]

let render (state: State) (dispatch: Msg -> Unit) =
    let content = 
        if state.Loading then Html.h1 "Loading.."
        else Html.h1 state.Count

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``            
        ]
        prop.children [
            SideNav.sideNavTemp
            
            Html.div [
                prop.classes [
                    tw.``flex-grow-3``
                ]
                prop.children [
                    main
                    content 
                    Html.button [
                        prop.onClick (fun _ -> dispatch Increment)
                        prop.text "Increment"
                    ]

                    Html.button [
                        prop.onClick (fun _ -> dispatch Decrement)
                        prop.text "Decrement"
                    ]

                    Html.button [
                        prop.onClick (fun _ -> dispatch IncrementDelayed)
                        prop.text "IncrementDelayed"
                    ]
                ]
            ]

            Html.div [
                prop.classes [
                    tw.``flex-grow-2``
                ]
                prop.children [
                    trending
                    Html.button [
                        prop.onClick (fun _ -> dispatch Increment)
                        prop.text "Increment"
                    ]
                ]
            ]
        ]
        //SideNav.render state dispatch
        
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run