[<RequireQualifiedAccess>]
module BleeterInfo

open Feliz
open Tailwind

let page = 
    Html.div [
        prop.classes [
            tw.``ml-3``
        ]
        prop.children [
            Html.h1 "Bleeter Info!!!!"    
        ]
    ]