[<RequireQualifiedAccess>]
module MainLayout

open Feliz
open Tailwind

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

let banner (url: string option) (height: int) =
    Html.img [
        prop.classes [
            tw.``w-full``
            tw.``h-auto``
            (if url.IsSome then
                 tw.``bg-cover``
             else
                 tw.``bg-transparent``)
        ]
        // prop.style [
        //     style.minHeight height
        //     style.maxHeight height
        // ]
        prop.src (
            match url with
            | Some url -> url
            | None -> ""
        )
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

    let children = [ banner imgUrlOpt 160; coreComponentsElem ]

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
            prop.children [
                Html.div [
                    prop.classes [
                        tw.flex
                        tw.``flex-row``
                        tw.``mx-auto``
                        tw.``w-full``
                        tw.``h-full``
                        tw.``justify-center``
                    ]
                    prop.children coreComponents
                ]
            ]
        ]

    let children = [ banner imgUrlOpt 160; coreComponentsElem ]

    commonLayout children
