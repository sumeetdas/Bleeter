[<RequireQualifiedAccess>]
module EllipsisOption

open Elmish
open Feliz
open Tailwind
// for `?` operator
open Fable.Core.JsInterop

type 'a Msg =
    | Close
    | Open of Coordinates
    | CommandMsg of 'a Cmd

type 'a Option = { Name: string; Command: 'a Cmd }

type 'a State =
    {
        IsOptionOpen: bool
        Coordinates: Coordinates
        Options: 'a Option list
        Size: int
        CssClasses: string list
        Offset: Coordinates
    }

type Size = { CssClasses: string list; IconSize: string }

let sizes: Map<int, Size> =
    Map
        .empty
        .Add(
            20,
            {
                CssClasses = [ tw.``h-10``; tw.``w-10``; tw.``mt-3``; tw.``p-2.5`` ]
                IconSize = "20"
            }
        )
        .Add(
            16,
            {
                CssClasses =
                    [
                        tw.``w-8``
                        tw.``h-8``
                        tw.``p-1``
                        tw.``pl-2``
                        tw.``pt-2``
                    ]
                IconSize = "16"
            }
        )
        .Add(
            12,
            {
                CssClasses = [ tw.``w-5``; tw.``h-5``; tw.``p-1`` ]
                IconSize = "12"
            }
        )

let init options (size: int) (cssClasses: string list) (offset: Coordinates) =
    {
        IsOptionOpen = false
        Coordinates = { X = 0 |> float; Y = 0 |> float }
        Options = options
        Size = size
        CssClasses = cssClasses
        Offset = offset
    }

let update (msg: 'a Msg) (state: 'a State) : 'a State * 'a Cmd =
    match msg with
    | CommandMsg cmd -> state, cmd
    | Close -> { state with IsOptionOpen = false }, Cmd.none
    | Open coordinates ->
        printf "Bleet option open %A" coordinates
        
        { state with
            IsOptionOpen = true
            Coordinates = coordinates
        },
        Cmd.none

let render (state: 'a State) (dispatch: 'a Msg -> unit) =
    let optionList =
        state.Options
        |> List.map
            (fun opt ->
                Html.div [
                    prop.classes [
                        tw.``h-10``
                        tw.``border-b``
                        tw.flex
                        tw.``items-center``
                        tw.``pl-4``
                        tw.``hover:bg-gray-300``
                        tw.``cursor-pointer``
                    ]
                    prop.text opt.Name
                    prop.onClick (fun _ -> dispatch (CommandMsg opt.Command))
                ])

    let optionsElem =
        Html.div [
            prop.children [
                Html.div [
                    prop.onClick (fun _ -> dispatch (Close))
                    prop.classes [
                        tw.``fixed``
                        tw.``inset-0``
                        tw.``h-full``
                        tw.``w-full``
                        (if state.IsOptionOpen then tw.block else tw.hidden)
                    ]
                ]
                Html.div [
                    prop.style [
                        style.top (state.Coordinates.Y |> int)
                        style.left (state.Coordinates.X |> int)
                    ]
                    prop.classes [
                        tw.``w-36``
                        tw.``bg-white``
                        tw.``rounded-md``
                        tw.``overflow-hidden``
                        tw.``shadow-xl``
                        tw.border
                        tw.``border-solid``
                        tw.flex
                        tw.``flex-col``
                        (if state.IsOptionOpen then tw.absolute else tw.hidden)
                    ]
                    prop.children optionList
                ]
            ]
        ]

    let renderedElem size =
        let classes = [ size.CssClasses; state.CssClasses ] |> List.concat

        Html.div [
            Html.div [
                prop.onClick
                    (fun event ->
                        // https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget
                        let coordinates =
                            {
                                X = event.currentTarget?offsetLeft
                                Y = event.currentTarget?offsetTop
                            }

                        let coordinates =
                            {
                                X = coordinates.X + state.Offset.X
                                Y = coordinates.Y + state.Offset.Y
                            }

                        dispatch (Open coordinates))
                prop.classes classes
                prop.children [
                    Bleeter.icon "ant-design:ellipsis-outlined" (size.IconSize)
                ]
            ]
            optionsElem
        ]

    match sizes.TryFind state.Size with
    | Some size -> renderedElem size
    | None -> Html.div []
