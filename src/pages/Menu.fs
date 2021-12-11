[<RequireQualifiedAccess>]
module Menu

open Feliz
open Tailwind

type Nav = 
    {
        IconName: string
        Text: string
        Url: string
        HideOnLargeScreen: bool
    }

let navList =
    [
        { IconName = "ant-design:home-outlined"; Text = "Home"; Url = "#/home"; HideOnLargeScreen = false }
        { IconName = "bx:bx-user-circle"; Text = "Profile"; Url = "#/bleeter"; HideOnLargeScreen = false }
        { IconName = "codicon:github"; Text = "Github"; Url = "https://www.github.com/sumeetdas/Bleeter"; HideOnLargeScreen = false }
        { IconName = "akar-icons:info"; Text = "Bleeter"; Url = "#/bleeter-info"; HideOnLargeScreen = false }
        { IconName = "ant-design:search-outlined"; Text = "Search"; Url = "#/search"; HideOnLargeScreen = true }
        { IconName = "akar-icons:hashtag"; Text = "Explore"; Url = "#/explore"; HideOnLargeScreen = true }
    ]

let nav (currentUrl: string) (nav: Nav) =
    let isSelected = currentUrl = (nav.Url.Replace("/", "").Replace("#", ""))

    Html.a [
        prop.classes ([
            tw.``ml-4``
            tw.``mt-1``
            tw.group
            tw.flex
            tw.``items-center``
            tw.``px-2``
            tw.``py-2``
            tw.``text-gray-100``
            tw.``text-base``
            tw.``leading-5``
            tw.``font-medium``
            tw.``rounded-full``
            tw.``hover:bg-bleeter-blue-hover``
            tw.``w-12``
            tw.``md:w-56``
            (if nav.HideOnLargeScreen then tw.``lg:hidden`` else tw.``block``)
        ] @ (if isSelected then [ tw.``font-black``; tw.``border``; tw.``border-4`` ] else [ tw.``font-normal`` ]))
        prop.target (if (nav.Url |> String.contains "http") then "_blank" else "")
        prop.href nav.Url
        prop.children [
            Html.div [
                prop.classes [ 
                    tw.``mr-4``
                    tw.``hidden``
                    tw.``md:block``
                ]
                prop.children [
                    Bleeter.bigIcon nav.IconName
                ]
            ]
            Html.div [
                prop.classes [ 
                    tw.``mr-4``
                    tw.``block``
                    tw.``md:hidden``
                ]
                prop.children [
                    Bleeter.icon nav.IconName "32"
                ]
            ]
            Html.div [
                prop.classes [
                    tw.``hidden``
                    tw.``md:block``
                ]
                prop.text nav.Text
            ]
        ]
    ]

let bleeterIcon =
    let commonCss = 
        [
            tw.``py-2``
            tw.``px-2``
            tw.``mt-1``
            tw.``text-gray-100``
            tw.``hidden``
            tw.``md:block``
            tw.``ml-4``
        ]

    let iconElem (css: string list) (icon: ReactElement): ReactElement =  
        Html.div [
            prop.classes (commonCss @ css)
            prop.children [
                icon
            ]
        ]

    Html.a [
        prop.href ""
        prop.children [
            iconElem ([tw.``hidden``;tw.``md:block``]) (Bleeter.bigIcon "mdi:sheep")
            iconElem ([tw.``block``; tw.``md:hidden``]) (Bleeter.icon "mdi:sheep" "32")
        ]
    ]

let bleetButton =
    let commonCss = 
        [
            tw.``bg-green-400``
            tw.``hover:bg-green-500``
            tw.``mt-5``
            tw.``text-white``
            tw.``font-bold``
            tw.``rounded-full``
            tw.``leading-5``
            tw.``text-lg``
        ]

    let buttonElem (css: string list) (text: ReactElement): ReactElement = 
        Html.button [
            prop.classes (commonCss @ css)
            prop.children [
                text
            ] 
        ]

    Html.a [
        prop.href "#/create/bleet"
        prop.children [
            buttonElem [
                tw.``w-full``
                tw.``h-12``
                tw.``py-2``
                tw.``px-4``
                tw.``hidden``
                tw.``md:block``
            ] (Html.text "Bleet")
            buttonElem [
                tw.``py-2``
                tw.``px-2``
                tw.``block``
                tw.``md:hidden``
                tw.``ml-4``
            ] (Bleeter.icon "mdi:sheep" "32")
        ]
    ]

let menuHtml (height: int) (currentUrl: string list) =
    let urlString =
        currentUrl
        |> Seq.map (fun elem -> elem.ToLower())
        |> String.concat ","

    let navList = navList |> List.map (nav urlString)

    Html.div [
        prop.classes [
            tw.``md:flex-grow-2``
            tw.``flex``
            tw.``flex-row``
            tw.``justify-end``
            tw.``md:flex-1``
            tw.``w-16``
            tw.``md:w-max``
            tw.``mr-4``
        ]
        prop.style [ style.height height ]
        prop.children [
            Html.div [
                prop.classes [ 
                    tw.``flex``
                    tw.``flex-col``
                ]
                prop.style [ style.width 250 ]
                prop.children (
                    List.concat [
                        [ bleeterIcon ]
                        navList
                        [ bleetButton ]
                    ]
                )
            ]
        ]
    ]
