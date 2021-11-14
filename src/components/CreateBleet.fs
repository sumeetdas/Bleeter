[<RequireQualifiedAccess>]
module CreateBleet

open Feliz
open Tailwind
open Browser
open Browser.Types

type State = { display: bool; isModal: bool; Profile: Profile option; Bleet: Bleet option}

type Msg = 
    | DisplayWeb of Profile
    | DisplayModal of Profile
    | Close
    | AddBleet of Bleet

let init() = { display = false; isModal = false; Profile = None; Bleet = None }

let update (msg:Msg) (state:State) : State =
    match msg with 
    | DisplayWeb profile -> {state with display = true; isModal = false; Profile = Some profile}
    | DisplayModal profile -> {state with display = true; isModal = true; Profile = Some profile}
    | Close -> {display = false; isModal = false; Profile = None; Bleet = None}
    | AddBleet bleet -> 
        let bleet = if state.Profile.IsNone then None else Some bleet
        {state with Bleet = bleet; Profile = None}

let mainElem (profile:Profile option) (dispatch: Msg -> unit) =
    let bleetBtnClick = fun _ -> 
        let bleetText = ((document.getElementById "create-bleet-text") :?> HTMLTextAreaElement).value
        let profile = profile |> Option.defaultWith (fun () -> Profile.init())
        let bleet = {
            Name = profile.Name
            Content = bleetText
            ProfilePic = profile.ProfilePic
            Handle = profile.Handle
            Time = ""
            Rebleets = 0
            Likes = 0
            Replies = 0
        }
        dispatch (AddBleet bleet)
    
    Html.div [
        prop.classes [
            tw.``flex``
        ]
        prop.children [
            Html.div [
                prop.classes [

                ]
                prop.children [
                    Html.img [
                        prop.classes [
                            
                        ]
                        prop.href (profile |> Option.fold (fun _ p -> p.Url) "")
                    ]
                ]
            ]
            Html.div [
                Html.textarea [
                    prop.id "create-bleet-text"
                    prop.classes []
                    prop.placeholder "Mehehe?"
                ]
                Html.button [
                    prop.id "create-bleet-button"
                    prop.classes []
                    prop.onClick bleetBtnClick
                    prop.text "Bleet"
                ]
            ]
        ]
    ]

let render (state:State) (dispatch: Msg -> unit) = 
    Html.div [
        prop.classes [
            tw.``ml-3``
        ]
        prop.children [
            Html.h1 "Bleeter Info!!!!"    
            (mainElem state.Profile dispatch)
        ]
    ]