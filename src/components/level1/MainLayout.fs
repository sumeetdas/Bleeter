[<RequireQualifiedAccess>]
module MainLayout

open Feliz
open Tailwind
open System

let heading (name: string) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``
            tw.``h-20``
            tw.``bg-gray-100``
            tw.``w-full``
            tw.``text-2xl``
            tw.``text-green-600``
            tw.``items-center``
            tw.``border-b``
            tw.``border-gray-300``
        ]
        prop.style [ style.minHeight 75 ]
        prop.children [
            Html.div [
                prop.classes [ tw.``ml-4`` ]
                prop.text name
            ]
        ]
    ]

let banner (url: string) (height: int) =
    Html.img [
        prop.classes [
            tw.``w-full``
            tw.``bg-cover``
        ]
        prop.style [
            style.minHeight height
            style.maxHeight height
        ]
        prop.src url
    ]

let transparentBackground (height: int) =
    Html.div [
        prop.classes [
            tw.``w-full``
            tw.``bg-transparent``
        ]
        prop.style [ style.minHeight height ]
    ]

let commonLayout (children: ReactElement list) =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
            tw.``h-full``
            tw.``w-full``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                    tw.``h-full``
                ]
                prop.children children
            ]
        ]
    ]

let elem (imgUrlOpt: string option) (coreComponents: ReactElement list) =
    let coreComponentsElem =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-col``
                tw.``bg-gray-100``
                tw.``h-full``
            ]
            prop.children coreComponents
        ]

    let children =
        [
            (match imgUrlOpt with
             | Some url -> banner url 160
             | None -> transparentBackground 160)
            coreComponentsElem
        ]

    commonLayout children

let mobileElem (imgUrlOpt: string option) (coreComponents: ReactElement list) =
    let coreComponentsElem =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-col``
                tw.``bg-gray-100``
                tw.``h-full``
                tw.``w-full``
            ]
            prop.children coreComponents
        ]

    let height =
        let width = Bleeter.getWindowWidth ()
        let diff = (width - 320) / 5
        Math.Min(160, Math.Max(80, 80 + diff))

    let children =
        [
            (match imgUrlOpt with
             | Some url -> banner url height
             | None -> transparentBackground height)
            coreComponentsElem
        ]

    commonLayout children
