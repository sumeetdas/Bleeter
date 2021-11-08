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

type Profile = {Name: string; ProfilePic: string; Banner: string; Handle: string; Following: int; Followers: int}

type Bleet = {Name: string; Content: string; ProfilePic: string; Handle: string; Time: string; Rebleets: int; Likes: int; Replies: int}

let bleets = [
    {Name= "Bleeter Boi"; Content= "Hello Bleeter!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "BleeterBoi"; Time = ""; Rebleets = 123; Likes = 3000; Replies = 0}
    {Name= "Sheeple"; Content= "We the Sheeple!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "Sheeple"; Time = ""; Rebleets = 1230; Likes = 40000; Replies = 0}
]

let bleetElem (bleet:Bleet) = 
    Html.div [
        prop.classes []
        prop.children [
            Html.div [
                prop.classes []
                prop.children [
                    Html.img [
                        prop.classes [
                            tw.``h-12``
                            tw.``w-12``
                            tw.``rounded-full``
                            tw.``border-2``
                            tw.``border-indigo-400``
                        ]
                        prop.src bleet.ProfilePic
                    ]
                ]
            ]
            Html.div [ 
                prop.classes []
                prop.children [
                    Html.div [
                        prop.classes []
                        prop.children [
                            Html.span [
                                prop.text bleet.Name
                            ]
                            Html.span [
                                prop.text ("@" + bleet.Handle)
                            ]
                            Menu.icon "bi:dot" "16"
                            Menu.icon "ant-design:ellipsis-outlined" "16"
                        ]
                    ]
                    Html.span [
                        prop.text bleet.Content
                    ]
                    Html.div [ 
                        Html.div [
                            Menu.icon "ei:comment" "24"
                            Html.span [
                                prop.text bleet.Replies
                            ]
                        ]
                        Html.div [
                            Menu.icon "ei:retweet" "24"
                            Html.span [
                                prop.text bleet.Rebleets
                            ]
                        ]
                        Html.div [
                            Menu.icon "ei:heart" "24"
                            Html.span [
                                prop.text bleet.Likes
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

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
            Html.div [
                prop.children (bleets |> List.map bleetElem)
            ]
        ]
    ]

let searchBox = 
    Html.div [ 
        prop.classes []
        prop.children [
            Html.div [ 
                Menu.icon "ant-design:search-outlined" "24"
            ]
            Html.input [
                prop.placeholder "Search Bleeter"
            ]
            Html.div [ 
                prop.classes [ tw.``text-indigo-400`` ]
                prop.children [
                    Menu.icon "gridicons:cross-circle" "24"
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
        prop.children [
            Html.h3 [
                prop.text "Trending"
            ]
            Html.div [
                prop.children (trendList |> List.map elem)
            ]  
        ] 
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
            Menu.menuHtml
            
            Html.div [
                prop.classes [
                    tw.``flex-grow-1``
                    tw.``max-w-screen-sm``
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
                    Html.button [
                        prop.onClick (fun _ -> dispatch Increment)
                        prop.text "Increment"
                    ]
                ]
            ]
        ]        
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run