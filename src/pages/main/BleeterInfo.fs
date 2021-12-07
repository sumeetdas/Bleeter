[<RequireQualifiedAccess>]
module BleeterInfo

open Feliz
open Tailwind

let page =
    let coreComponents = [ MainLayout.heading "Bleeter Info!!!!" ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
