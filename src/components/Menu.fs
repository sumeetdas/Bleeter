[<RequireQualifiedAccess>]
module Menu

open Elmish
open Feliz
open Tailwind

type State = { Count : int }

type Msg = 
| In

let init() = { Count = 0 }

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with 
    | In -> state, Cmd.none

let icon (name:string) (size:string) =
    Html.span [
        prop.className "iconify-inline" 
        prop.custom ("data-icon", name)
        prop.custom ("data-width", size)
        prop.custom ("data-height", size)
    ]

let bigIcon (name:string) = icon name "48"

let nav (iconName:string, text:string, url: string) = 
    Html.a [
        prop.classes [
            tw.``mt-1``
            tw.``group``
            tw.``flex``
            tw.``items-center``
            tw.``px-2``
            tw.``py-2``
            tw.``text-base``
            tw.``leading-5``
            tw.``font-medium``
            tw.``rounded-full``
            tw.``hover:bg-gray-300``
            tw.``hover:text-green-800``
        ]
        prop.href url
        prop.children [
            Html.div [
                prop.classes [
                    tw.``mr-4``
                ]
                prop.children [
                    bigIcon iconName
                ]
            ]
            Html.text text
        ]
    ]

let menuHtml = 
    let navList = [
        ("ant-design:home-outlined", "Home", "#/home")
        ("akar-icons:hashtag", "Explore", "#/explore")
        ("bx:bx-user-circle", "Profile", "#/profile")
    ] 
    let navList = navList |> List.map nav
    let bleeterIcon = (bigIcon "mdi:sheep")
    let bleetButton = 
        Html.button [
            prop.classes [
                tw.``bg-green-400`` 
                tw.``hover:bg-green-500``
                tw.``w-full``
                tw.``mt-5``
                tw.``text-white``
                tw.``font-bold``
                tw.``py-2``
                tw.``px-4``
                tw.``rounded-full``
                tw.``leading-5``
                tw.``h-12``
                tw.``text-lg``
            ]
            prop.text "Bleet"
        ]

    Html.div [
        prop.classes [
            tw.``flex-grow-2``
            tw.``flex-1``
            tw.``w-max``
            tw.``mr-4``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.``float-right``
                ]
                prop.style [
                    style.width 250
                ]
                prop.children (List.concat [[bleeterIcon]; navList; [bleetButton]])
            ]
        ] 
    ]

let render (state: State) (dispatch: Msg -> Unit) =
    Html.div [
        bigIcon "mdi:sheep"
        Html.div [

        ]
    ]

