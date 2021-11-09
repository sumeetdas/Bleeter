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

type Profile = {Name: string; ProfilePic: string; Banner: string; Handle: string; Following: int; Followers: int; Location: string; Url: string}

type Bleet = {Name: string; Content: string; ProfilePic: string; Handle: string; Time: string; Rebleets: int; Likes: int; Replies: int}

let bleets = [
    {Name= "Bleeter Boi"; Content= "Hello Bleeter!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "BleeterBoi"; Time = ""; Rebleets = 123; Likes = 3000; Replies = 0}
    {Name= "Sheeple"; Content= "We the Sheeple!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "Sheeple"; Time = ""; Rebleets = 1230; Likes = 40000; Replies = 0}
]

let bleetElem (bleet:Bleet) = 
    Html.article [
        prop.classes [
            tw.``hover:bg-gray-300``
            tw.``flex``
            tw.``flex-row``
            tw.``p-4``
            tw.``pb-0``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.``flex``
                    tw.``flex-grow-0``
                    tw.``w-12``
                ]
                prop.children [
                    Html.img [
                        prop.classes [
                            tw.``h-12``
                            tw.``w-12``
                            tw.``rounded-full``
                            tw.``border-2``
                            tw.``border-gray-100``
                        ]
                        prop.src bleet.ProfilePic
                    ]
                ]
            ]
            Html.div [ 
                prop.classes [
                    tw.``flex``
                    tw.``flex-col``
                    tw.``flex-grow-1``
                    tw.``pl-2``
                ]
                prop.children [
                    Html.div [
                        prop.classes [
                            tw.``flex``
                            tw.``flex-row``
                        ]
                        prop.children [
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-grow-1``
                                ]
                                prop.children [
                                    Html.span [
                                        prop.classes [
                                            tw.``hover:underline``
                                        ]
                                        prop.text bleet.Name
                                    ]
                                    Html.span [
                                        prop.text ("@" + bleet.Handle)
                                    ]
                                    Html.span [
                                        prop.classes [
                                            tw.``p-1``
                                        ]
                                        prop.children [
                                            Menu.icon "bi:dot" "16"
                                        ]
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``float-right``
                                    tw.``w-8``
                                    tw.``p-1``
                                ]
                                prop.children [
                                    Menu.icon "ant-design:ellipsis-outlined" "16"
                                ]
                            ]
                        ]
                    ]
                    Html.div [
                        prop.classes [ 
                            tw.``flex``
                        ]
                        prop.children [
                            Html.span [
                                prop.text bleet.Content
                            ]
                        ]
                    ]
                    Html.div [ 
                        prop.classes [
                            tw.``flex``
                            tw.``py-4``
                        ]
                        prop.children [
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-1``
                                ]
                                prop.children [
                                    Menu.icon "ei:comment" "24"
                                    Html.text bleet.Replies
                                ]
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-1``
                                ]
                                prop.children [
                                    Menu.icon "ei:retweet" "24"
                                    Html.text bleet.Rebleets
                                ]
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-1``
                                ]
                                prop.children [
                                    Menu.icon "ei:heart" "24"
                                    Html.text bleet.Likes
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let bleetProfileElem (profile: Profile) = 
    Html.div [ 
        Html.img [
            prop.classes [
                tw.``w-full``
                tw.``bg-cover``
            ]
            prop.src profile.Banner
        ]
        Html.img [
            prop.classes [
                tw.``h-24``
                tw.``w-24``
                tw.``rounded-full``
                tw.``border-4``
                tw.``border-gray-100``
            ]
            prop.src profile.ProfilePic
        ]
        Html.div [
            prop.classes [
                tw.``flex``
            ]
            prop.children [
                Html.span [ 
                    prop.text profile.Name
                ]
            ]
        ]
        Html.div [
            prop.classes [
                tw.``flex``
            ]
            prop.children [
                Html.span [ 
                    prop.text ("@" + profile.Handle)
                ]
            ]
        ]
        Html.div [
            prop.classes [
                tw.``flex``
                tw.``flex-row``
            ]
            prop.children [
                Html.div [
                    prop.classes [
                        tw.``flex``
                        tw.``flex-row``
                    ]
                    prop.children [
                        Menu.icon "akar-icons:location" "12"
                        Html.span [ 
                            prop.text profile.Location
                        ]
                    ]
                ]
                Html.div [
                    prop.classes [
                        tw.``flex``
                        tw.``flex-row``
                    ]
                    prop.children [
                        Menu.icon "il:url" "12"
                        Html.span [ 
                            prop.text profile.Url
                        ]
                    ]
                ]
            ]
        ]
        Html.div [
            prop.classes [
                tw.``flex``
                tw.``flex-row``
            ]
            prop.children [
                Html.a [
                    prop.href "#"
                    prop.children [
                        Html.span [
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.text "Following"
                        ]
                    ]
                ]
                Html.a [
                    prop.href "#"
                    prop.children [
                        Html.span [
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.text "Followers"
                        ]
                    ]
                ]
            ]
        ]
    ]

let main = 
    Html.div [ 
        bleetProfileElem ({Name = "Bleeter"; ProfilePic = "/bleeter_profile_pic.png"; Banner = "/bleeter_banner.jpg"; Handle = "bleeter"; Following = 30; Followers = 24;
        Url = "sumeetdas.me/bleeter"; Location = "Hill"})

        Html.h2 [
            prop.text "Latest Bleets"
        ]

        Html.div [
            prop.children (bleets |> List.map bleetElem)
        ]
    ]

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
                    Menu.icon "ant-design:search-outlined" "24"
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
                        Menu.icon "ant-design:ellipsis-outlined" "12"
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
    let content = 
        if state.Loading then Html.h1 "Loading.."
        else Html.h1 state.Count

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``    
            tw.``bg-gray-100``        
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