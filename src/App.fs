module App

open Elmish
open Elmish.React
open Feliz
open Feliz.Router
open Tailwind
open Browser
// for `?` operator
open Fable.Core.JsInterop

// data model
type State =
    {
        Data: Data.State
        CurrentUrl: string list
        Main: Main.State
        CreateBleet: CreateBleet.State
        Distraction: Distraction.State
        SearchBox: SearchBox.State
    }

// events
type Msg =
    | DataMsg of Data.Msg
    | UrlChanged of string list
    | MainMsg of Main.Msg
    | CreateBleetMsg of CreateBleet.Msg
    | DistractionMsg of Distraction.Msg
    | SearchBoxMsg of SearchBox.Msg

// need parentheses for indicating that init is a function
let init () =
    let currentUrl = Router.currentUrl ()
    let data, dataCmd = Data.init ()
    let main, mainCmd = Main.init currentUrl data

    {
        Data = data
        CurrentUrl = currentUrl
        Main = main
        CreateBleet = CreateBleet.init ()
        Distraction = Distraction.init data
        SearchBox = SearchBox.init ()
    },
    Cmd.batch [
        (Cmd.map MainMsg mainCmd)
        (Cmd.map DataMsg dataCmd)
    ]

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | DataMsg msg' ->
        let data, dataCmd = Data.update msg' state.Data
        let main, mainCmd = Main.update (Main.Msg.DataUpdate data) state.Main
        let distraction, distractionCmd = Distraction.update (Distraction.Msg.DataUpdate data) state.Distraction

        { state with
            Data = data
            Main = main
            Distraction = distraction
        },
        Cmd.batch [
            (Cmd.map DataMsg dataCmd)
            (Cmd.map MainMsg mainCmd)
            (Cmd.map DistractionMsg distractionCmd)
        ]
    | UrlChanged url ->
        match url with
        | [ "create"; "bleet" ] ->
            match state.Data.MyProfile with
            | Some profile ->
                let createBleet, createBleetCmd =
                    CreateBleet.update (CreateBleet.Msg.DisplayModal(profile, state.CurrentUrl)) state.CreateBleet

                { state with CreateBleet = createBleet }, Cmd.map CreateBleetMsg createBleetCmd
            | None -> state, Cmd.none
        | _ ->
            let main, cmd = Main.update (Main.Msg.UrlChanged url) state.Main
            { state with CurrentUrl = url; Main = main }, (Cmd.map MainMsg cmd)
    | MainMsg msg' ->
        let main, cmd = Main.update msg' state.Main
        { state with Main = main }, (Cmd.map MainMsg cmd)
    | CreateBleetMsg msg' ->
        let createBleet, createBleetCmd = CreateBleet.update msg' state.CreateBleet

        match createBleet.Bleet with
        | None -> { state with CreateBleet = createBleet }, Cmd.none
        | Some bleet ->
            let data, dataCmd = Data.update (Data.Msg.AddBleet bleet) state.Data

            { state with Data = data; CreateBleet = createBleet },
            (Cmd.batch [
                Cmd.map DataMsg dataCmd
                Cmd.map CreateBleetMsg createBleetCmd
                Cmd.ofMsg (UrlChanged createBleet.PreviousUrl)
             ])
    | DistractionMsg msg ->
        let distraction, cmd = Distraction.update msg state.Distraction
        { state with Distraction = distraction }, (Cmd.map DistractionMsg cmd)
    | SearchBoxMsg msg ->
        let searchBox = SearchBox.update msg state.SearchBox
        { state with SearchBox = searchBox }, Cmd.none

let render (state: State) (dispatch: Msg -> Unit) =
    let page =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``min-h-full``
                tw.``h-full``
            ]
            prop.children [
                Menu.menuHtml

                (Main.render state.Main (MainMsg >> dispatch))

                Html.div [
                    prop.classes [ tw.``flex-grow-1`` ]
                    prop.children [
                        SearchBox.render state.SearchBox (SearchBoxMsg >> dispatch)
                        Distraction.render state.Distraction (DistractionMsg >> dispatch)
                    ]
                ]
                (CreateBleet.render state.CreateBleet (CreateBleetMsg >> dispatch))
            ]
        ]

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)
        router.children page
    ]

let appOnLoadHeight initial =
    let sub dispatch =
        window.addEventListener (
            "load",
            fun _ ->
                let scrollHeight =
                    (document.getElementById "elmish-app")
                        .scrollHeight
                    |> int

                let windowHeight = window.innerHeight |> int

                let finalHeight =
                    if scrollHeight > windowHeight then
                        scrollHeight
                    else
                        windowHeight

                (Main.Msg.AppHeight >> MainMsg >> dispatch) finalHeight
        )

    Cmd.ofSub sub

let appOnResizeHeight initial =
    let sub dispatch =
        window.addEventListener (
            "resize",
            fun _ ->
                let scrollHeight =
                    (document.getElementById "elmish-app")
                        .scrollHeight
                    |> int

                let windowHeight = window.innerHeight |> int

                let finalHeight =
                    if scrollHeight > windowHeight then
                        scrollHeight
                    else
                        windowHeight

                (Main.Msg.AppHeight >> MainMsg >> dispatch) finalHeight
        )

    Cmd.ofSub sub

// Subscriptions for DOM events doesn't work.
// GitHub Issue: https://github.com/elmish/elmish/issues/229
// let followClick initial =
//     let sub dispatch = window.addEventListener("load", fun _ ->
//         let followButton = document.getElementById "bleeter-follow"
//         followButton.addEventListener("onClick", fun _ ->
//             printf "yolo"
//         )
//     )
//     Cmd.ofSub sub

let subscribers initial =
    let currentUrl = Router.currentUrl ()

    Cmd.batch [
        appOnLoadHeight initial
        appOnResizeHeight initial
        Cmd.ofMsg (UrlChanged(if currentUrl.Length = 0 then [ "home" ] else currentUrl))
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription subscribers
|> Program.run
