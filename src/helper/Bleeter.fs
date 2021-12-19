[<RequireQualifiedAccess>]
module Bleeter

open Feliz
open Browser.Dom
open Tailwind

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

let getWindowWidth () = window.innerWidth |> int

let isMobile () =
    let width = getWindowWidth ()
    width <= 640

// https://youtu.be/HrcbCW4y9Dw?t=14
let numberToWoman (number: int) : string =
    let getString (exponent: int) (prefix: char) (number: int) =
        let num = (number * 10) / exponent

        match num with
        | num when num < 100 && num % 10 <> 0 -> sprintf "%.1f%c" ((num |> float) / 10.0) prefix
        | num -> sprintf "%d%c" (num / 10) prefix

    match number with
    | num when num >= 1_000_000 -> num |> getString 1_000_000 'M'
    | num when num >= 1_000 -> num |> getString 1_000 'K'
    | num -> sprintf "%d" num

let ytEmbed (src: string) =
    Html.div [
        prop.classes [ tw.``iframe-container`` ]
        prop.children [
            Html.iframe [
                prop.custom ("data-src", src)
                prop.src src
                prop.custom ("frameBorder", "0")
                prop.custom ("allow", "accelerometer; autoplay")
                prop.custom ("allowFullScreen", true)
            ]
        ]
    ]
