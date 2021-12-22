[<RequireQualifiedAccess>]
module Iconify

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

let private iconifyPreload: obj list = import "IconifyPreload" "./iconify-preload.js"
let private addCollection (icon: obj) : unit = import "addCollection" "@iconify/react"

let loadAllIcons() =
    iconifyPreload 
    |> List.map (
        fun icon -> 
            addCollection icon)
    |> ignore

type IconifyProps = 
    | Icon of string
    | Width of string
    | Height of string

let inline icon (props : IconifyProps list) (elems : ReactElement list) : ReactElement =
    ofImport "Icon" "@iconify/react" (keyValueList CaseRules.LowerFirst props) elems