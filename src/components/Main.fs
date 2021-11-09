[<RequireQualifiedAccess>]
module Main

open Elmish
open Feliz
open Tailwind

type Profile = {Name: string; ProfilePic: string; Banner: string; Handle: string; Following: int; Followers: int; Location: string; Url: string}

type Bleet = {Name: string; Content: string; ProfilePic: string; Handle: string; Time: string; Rebleets: int; Likes: int; Replies: int}

let bleets = [
    {Name= "Bleeter Boi"; Content= "Hello Bleeter!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "BleeterBoi"; Time = ""; Rebleets = 123; Likes = 3000; Replies = 0}
    {Name= "Sheeple"; Content= "We the Sheeple!"; ProfilePic= "/bleeter_profile_pic.png"; Handle = "Sheeple"; Time = ""; Rebleets = 1230; Likes = 40000; Replies = 1}
]

let bleetElem (bleet:Bleet) = 
    Html.article [
        prop.classes [
            tw.``hover:bg-gray-300``
            tw.``flex``
            tw.``flex-row``
            tw.``p-4``
            tw.``pb-0``
            tw.``border-b``
            tw.``border-gray-300``
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
                                            Bleeter.icon "bi:dot" "16"
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
                                    Bleeter.icon "ant-design:ellipsis-outlined" "16"
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
                                    Bleeter.icon "ei:comment" "24"
                                    Html.text bleet.Replies
                                ]
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-1``
                                ]
                                prop.children [
                                    Bleeter.icon "ei:retweet" "24"
                                    Html.text bleet.Rebleets
                                ]
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-1``
                                ]
                                prop.children [
                                    Bleeter.icon "ei:heart" "24"
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
                tw.``block``
                tw.``m-2``
            ]
            prop.children [
                Html.div [
                    prop.classes [
                        tw.``inline-flex``
                    ]
                    prop.children [
                        Html.div [
                            prop.classes [
                                tw.``flex-1``
                                tw.``pt-1``
                            ]
                            prop.children [
                                Bleeter.icon "akar-icons:location" "16"
                            ]
                        ]
                        Html.div [
                            prop.classes [
                                tw.``flex-1``
                            ]
                            prop.children [
                                Html.span [ 
                                    prop.text profile.Location
                                ]
                            ]
                        ]
                    ]
                ]
                Html.div [
                    prop.classes [
                        tw.``inline-flex``
                        tw.``ml-2``
                    ]
                    prop.children [
                        Html.div [
                            prop.classes [
                                tw.``flex-1``
                                tw.``pt-1``
                            ]
                            prop.children [
                                Bleeter.icon "il:url" "12"
                            ]
                        ]
                        Html.div [
                            prop.classes [
                                tw.``flex-1``
                            ]
                            prop.children [
                                Html.span [ 
                                    prop.text profile.Url
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Html.div [
            prop.classes [
                tw.``flex``
                tw.``flex-row``
                tw.``m-2``
            ]
            prop.children [
                Html.a [
                    prop.href "#"
                    prop.children [
                        Html.span [
                            prop.classes [
                                tw.``font-bold``
                            ]
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.classes [
                                tw.``ml-1``
                            ]
                            prop.text "Following"
                        ]
                    ]
                ]
                Html.a [
                    prop.classes [
                        tw.``ml-2``
                    ]
                    prop.href "#"
                    prop.children [
                        Html.span [
                            prop.classes [
                                tw.``font-bold``
                            ]
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.classes [
                                tw.``ml-1``
                            ]
                            prop.text "Followers"
                        ]
                    ]
                ]
            ]
        ]
    ]

let mainElem = 
    Html.div [ 
        prop.classes [
            tw.``border-l``
            tw.``border-r``
        ]
        prop.children [
            bleetProfileElem ({Name = "Bleeter"; ProfilePic = "/bleeter_profile_pic.png"; Banner = "/bleeter_banner.jpg"; Handle = "bleeter"; Following = 30; Followers = 24;
            Url = "sumeetdas.me/bleeter"; Location = "Hill"})

            Html.div [
                prop.classes [
                    tw.``text-2xl``
                    tw.``h-12``
                    tw.``border-b``
                    tw.``border-gray-300``
                    tw.``text-green-600``
                ]
                prop.children [
                    Html.span [
                        prop.classes [
                            tw.``m-6``
                        ]
                        prop.text "Latest Bleets"
                    ]
                ]
            ]

            Html.div [
                prop.children (bleets |> List.map bleetElem)
            ]
        ]
    ]
