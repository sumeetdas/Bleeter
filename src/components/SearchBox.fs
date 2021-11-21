[<RequireQualifiedAccess>]
module SearchBox

open Feliz
open Tailwind
open Browser.Types
// for `?` operator
open Fable.Core.JsInterop

type State = {Content: string}

type Msg = 
    | Clear 
    | Update of string

let init () = {Content = ""}

let update (msg:Msg) (state:State) : State =
    match msg with
    | Clear -> 
        {Content = ""}
    | Update content -> 
        {Content = content}

let render (state:State) (dispatch:Msg -> unit) =
    Html.div [
        prop.classes [
            tw.``bg-bleet-dim``
            tw.``text-gray-500``
            tw.``w-96``
            tw.``h-10``
            tw.``m-4``
            tw.``focus-within:ring``
            tw.``focus-within:border-blue-300``
            tw.``rounded-full``
            tw.flex
        ]
        prop.children [
            Html.button [
                prop.classes [ tw.``mx-3`` ]
                prop.children [
                    Bleeter.icon "ant-design:search-outlined" "24"
                ]
            ]
            Html.input [
                prop.classes [
                    tw.``bg-bleet-dim``
                    tw.``h-10``
                    tw.``w-72``
                    tw.``text-sm``
                    tw.``border-0``
                    tw.``focus:outline-none``
                ]
                prop.placeholder "Search Bleeter"
                prop.value state.Content
                prop.onChange
                    (fun (ev: Event) ->
                        dispatch (Update(ev.target?value |> string)))
            ]
            Html.button [
                prop.classes [
                    tw.``text-green-600``
                    tw.``mx-3``
                    (if state.Content.Length = 0 then tw.hidden else tw.block)
                ]
                prop.onClick (fun _ -> dispatch(Clear))
                prop.children [
                    Bleeter.icon "gridicons:cross-circle" "24"
                ]
            ]
        ]
    ]
