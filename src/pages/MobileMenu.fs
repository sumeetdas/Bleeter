[<RequireQualifiedAccess>]
module MobileMenu

open Feliz
open Tailwind
// for `?` operator
open Fable.Core.JsInterop

type State = { Display: bool; CurrentUrl: string list }

type Msg = 
    | Display
    | Close
    | OutsideModalClickClose of string

let init () = { Display = false; CurrentUrl = [] }

let update (msg: Msg) (state: State) : State = 
    match msg with 
    | Display -> { state with Display = true }
    | Close -> { state with Display = false }
    | OutsideModalClickClose id ->
        if (state.Display && id = "MobileMenu") then
            { state with Display = false }
        else
            state

let nav (currentUrl: string) (dispatch: Msg -> unit) (nav: Nav) =
    let isSelected = currentUrl = (nav.Url.Replace("/", "").Replace("#", ""))

    Html.a [
        prop.classes (
            [
                tw.``mt-1``
                tw.group
                tw.flex
                tw.``items-center``
                tw.``px-2``
                tw.``py-2``
                tw.``text-gray-100``
                tw.``text-base``
                tw.``leading-5``
                tw.``font-medium``
                tw.``rounded-full``
                tw.``hover:bg-bleeter-blue-hover``
                tw.``w-12``
            ]
            @ (if isSelected then
                   [ tw.``font-black``; tw.border; tw.``border-4`` ]
               else
                   [ tw.``font-normal`` ])
        )
        prop.target (if (nav.Url |> String.contains "http") then "_blank" else "")
        prop.href nav.Url
        prop.onClick (fun _ -> dispatch (Close))
        prop.children [
            Html.div [
                prop.classes [
                    tw.``mr-4``
                ]
                prop.children [
                    Bleeter.icon nav.IconName "32"
                ]
            ]
            Html.div [
                prop.text nav.Text
            ]
        ]
    ]

let closeButton (dispatch: Msg -> unit) = 
    Html.button [
        prop.classes [
            tw.flex
            tw.``flex-row``
            tw.``text-gray-100``
            tw.``px-2.5``
            tw.``py-2.5``
            tw.``hover:bg-bleeter-blue-hover``
            tw.``w-11``
            tw.``h-11``
            tw.``rounded-full``
        ]
        prop.children [
            Bleeter.icon "akar-icons:cross" "24"
        ]
        prop.onClick (fun _ -> dispatch (Close))
    ]

let navElems (urlString: string) (dispatch: Msg -> unit) = 
    let elem = 
        Menu.navList 
        |> List.map (nav urlString dispatch)
    
    Html.div [
        prop.classes [

        ]
        prop.children elem
    ]

let render (state: State) (dispatch: Msg -> unit) = 
    let urlString =
        state.CurrentUrl
        |> Seq.map (fun elem -> elem.ToLower())
        |> String.concat ","

    Html.div [
        prop.id "MobileMenu"
        prop.classes [
            tw.``fixed`` 
            tw.``inset-0`` 
            tw.``bg-gray-800`` 
            tw.``bg-opacity-75``
            tw.``z-50`` 
            (if state.Display then tw.``block`` else tw.``hidden``)
        ]
        prop.onClick (fun event -> dispatch (OutsideModalClickClose event.target?id))
        prop.children [
            Html.nav [
                prop.id "MobileMenuMain"
                prop.classes [
                    tw.``fixed``
                    tw.``top-0`` 
                    tw.``left-0`` 
                    tw.``bottom-0`` 
                    tw.``flex`` 
                    tw.``flex-col`` 
                    tw.``w-3/6`` 
                    tw.``max-w-sm`` 
                    tw.``py-6`` 
                    tw.``px-6`` 
                    tw.``bg-bleeter-blue`` 
                    tw.``border-r`` 
                    tw.``overflow-y-auto``
                    tw.``duration-300``
                ]
                prop.style (if state.Display then [ style.width 250 ] else [ style.width 0 ])
                prop.children [
                    Html.div [
                        prop.classes [
                            tw.``flex``
                            tw.``flex-row``
                            tw.``h-8``
                            tw.``mb-4``
                        ]
                        prop.children [
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-row``
                                ]
                                prop.children []
                            ]
                            Html.div [
                                prop.classes [
                                    tw.``flex``
                                    tw.``flex-row``
                                    tw.``float-right``
                                ]
                                prop.children [ closeButton dispatch ]
                            ]
                        ]
                    ]
                    navElems urlString dispatch
                ]
            ]
        ]
        
    ]
