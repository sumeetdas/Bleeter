[<RequireQualifiedAccess>]
module SearchBox

open Feliz
open Tailwind
open Browser.Types
// for `?` operator
open Fable.Core.JsInterop

type State = { Content: string; DoSearch: bool }

type Msg =
    | Clear
    | Update of string
    | DoSearch

let init () = { Content = ""; DoSearch = false }

let update (msg: Msg) (state: State) : State =
    match msg with
    | Clear -> init ()
    | Update content -> { state with Content = content }
    | DoSearch ->
        if state.Content.Length > 0 then
            { state with DoSearch = true }
        else
            state

let render (state: State) (dispatch: Msg -> unit) =
    let searchIconClasses =
        [
            [ tw.``mx-3`` ]
            (if not (state.Content.Length = 0) then
                 [ tw.``text-green-500``; tw.``font-extrabold`` ]
             else
                 [ tw.``text-gray-500`` ])
        ]
        |> List.concat

    Html.div [
        prop.classes [
            tw.``bg-gray-100``
            tw.``text-gray-500``
            tw.``w-64``
            tw.``sm:w-80``
            tw.``xl:w-96``
            tw.``h-10``
            tw.``m-4``
            tw.``focus-within:ring``
            tw.``focus-within:border-blue-300``
            tw.``rounded-full``
            tw.flex
            tw.``border-green-500``
            tw.``border-solid``
            tw.``lg:border-none``
            tw.``border-2``
            tw.``lg:border-0``
        ]
        prop.children [
            Html.button [
                prop.onClick (fun _ -> dispatch (DoSearch))
                prop.classes searchIconClasses
                prop.children [
                    Bleeter.icon "ant-design:search-outlined" "24"
                ]
            ]
            Html.input [
                prop.classes [
                    tw.``bg-gray-100``
                    tw.``h-9``
                    tw.``w-48``
                    tw.``sm:w-56``
                    tw.``xl:w-72``
                    tw.``text-sm``
                    tw.``border-0``
                    tw.``focus:outline-none``
                ]
                prop.onKeyPress (key.enter, (fun _ -> if state.Content.Length > 0 then dispatch (DoSearch)))
                prop.placeholder "Search Bleeter"
                prop.value state.Content
                prop.onChange (fun (ev: Event) -> dispatch (Update(ev.target?value |> string)))
            ]
            Html.button [
                prop.classes [
                    tw.``text-green-600``
                    tw.``mx-3``
                    (if state.Content.Length = 0 then tw.hidden else tw.block)
                ]
                prop.onClick (fun _ -> dispatch (Clear))
                prop.children [
                    Bleeter.icon "gridicons:cross-circle" "24"
                ]
            ]
        ]
    ]
