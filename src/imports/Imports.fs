module Imports

open Fable.Core.JsInterop

let iconifyPreload: obj list = import "IconifyPreload" "./iconify-preload.js"
let addCollection (icon: obj) : unit = import "addCollection" "@iconify/react"

let loadAllIcons() =
    printf "meow"
    iconifyPreload 
    |> List.map (
        fun icon -> 
            printf "icon obj %A" icon
            addCollection icon
    )
    |> ignore