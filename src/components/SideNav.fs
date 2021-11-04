[<RequireQualifiedAccess>]
module SideNav

open Elmish
open Feliz

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

let nav (iconName:string) (text:string) = 
    Html.a [
        Html.div [
            bigIcon iconName

            Html.div [
                Html.span [
                    prop.text text
                ]
            ]
        ]
    ]


let render (state: State) (dispatch: Msg -> Unit) =
    Html.div [
        bigIcon "mdi:sheep"
        Html.div [

        ]
    ]

