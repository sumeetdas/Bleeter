[<RequireQualifiedAccess>]
module SideNav

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
        prop.href url
        prop.children [
            Html.div [
                bigIcon iconName

                Html.div [
                    Html.span [
                        prop.text text
                    ]
                ]
            ]
        ]
    ]

let sideNavTemp = 
    let navList = [
        ("ant-design:home-outlined", "Home", "#/home")
        ("akar-icons:hashtag", "Explore", "#/explore")
        ("bx:bx-user-circle", "Profile", "#/profile")
    ] 
    let navList = navList |> List.map nav
    let bleeterIcon = (bigIcon "mdi:sheep")
    let bleetButton = 
        Html.button [
            prop.text "Bleet"
        ]

    Html.div [
        prop.classes [
            tw.``flex-grow-2``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.``float-right``
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

