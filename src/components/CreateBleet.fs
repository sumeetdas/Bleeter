[<RequireQualifiedAccess>]
module CreateBleet

open Elmish
open Feliz
open Tailwind
open Browser

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
    | AddBleet bleet -> {state with Bleet = Some bleet}

let createBleetSub model =
    let bleetText = (document.getElementById "create-bleet").innerText
    let profile = model.Profile |> Option.defaultWith (fun () -> Profile.init())
    let bleet = {
        Name = profile.Name
        Content = bleetText
        ProfilePic = profile.Url
        Handle = profile.Handle
        Time = ""
        Rebleets = 0
        Likes = 0
        Replies = 0
    }
    let sub dispatch = window.addEventListener("onClick", fun _ -> dispatch (AddBleet bleet))
    Cmd.ofSub sub

let mainElem (profile:Profile option) = 
    match profile with
    | None -> Html.div []
    | Some profile -> Html.div [
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
                        prop.href profile.Url
                    ]
                ]
            ]
            Html.div [
                Html.textarea [
                    prop.id "create-bleet"
                    prop.classes []
                    prop.placeholder "Mehehe?"
                ]
                Html.button [
                    prop.classes []
                    prop.text "Bleet"
                ]
            ]
        ]
    ]

let render (state:State) = 
    Html.div [
        prop.classes [
            tw.``ml-3``
        ]
        prop.children [
            Html.h1 "Bleeter Info!!!!"    
            (mainElem state.Profile)
        ]
    ]