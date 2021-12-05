[<RequireQualifiedAccess>]
module MainLayout

open Feliz
open Tailwind

let elem (urlOpt: string option) (coreComponents: ReactElement list) =
    let transparentBackground =
        Html.div [
            prop.classes [
                tw.``w-full``
                tw.``bg-transparent``
            ]
            prop.style [ style.minHeight 160 ]
        ]

    let imageBackground (url: string) =
        Html.img [
            prop.classes [
                tw.``w-full``
                tw.``bg-cover``
            ]
            prop.style [
                style.minHeight 160
                style.maxHeight 160
            ]
            prop.src url
        ]

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
            (match urlOpt with
             | Some url -> imageBackground url
             | None -> transparentBackground)
            coreComponentsElem
        ]

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
            tw.``h-full``
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
