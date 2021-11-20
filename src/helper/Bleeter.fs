[<RequireQualifiedAccess>]
module Bleeter

open Feliz

let icon (name: string) (size: string) =
    Html.span [
        prop.className "iconify-inline"
        prop.custom ("data-icon", name)
        prop.custom ("data-width", size)
        prop.custom ("data-height", size)
    ]

let bigIcon (name: string) = icon name "48"

let getUrl (url: string) =
    url
    |> String.replace "https" ""
    |> String.replace "//" ""
    |> String.replace ":" ""
