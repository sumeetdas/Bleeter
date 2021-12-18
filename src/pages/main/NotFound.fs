[<RequireQualifiedAccess>]
module NotFound

open Feliz
open Tailwind

let render =
    let header = MainLayout.heading "Page Not Found"

    let coreComponent =
        Html.iframe [
            prop.src "https://giphy.com/embed/l3q2ICbdSmDqnlxC0"
            prop.width 480
            prop.height 360
            // prop.frameBorder 0
            // prop.allowFullScreen
            prop.classes [ "giphy-embed"; tw.``w-full`` ]
        ]

    MainLayout.elem (Some "/img/bleeter-logo.png") [ header; coreComponent ]
