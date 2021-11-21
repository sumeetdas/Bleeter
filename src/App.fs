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
        CurrentUrl: string list
        Main: Main.State
        CreateBleet: CreateBleet.State
        Trending: Trending.State
        SearchBox: SearchBox.State
    }

// events
type Msg =
    | UrlChanged of string list
    | MainMsg of Main.Msg
    | CreateBleetMsg of CreateBleet.Msg
    | TrendingMsg of Trending.Msg
    | SearchBoxMsg of SearchBox.Msg

// need parentheses for indicating that init is a function
let init () =
    {
        CurrentUrl = Router.currentUrl ()
        Main = Main.init ()
        CreateBleet = CreateBleet.init ()
        Trending = Trending.init ()
        SearchBox = SearchBox.init ()
    },
    Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | UrlChanged url ->
        if (url |> List.contains "create") then
            let create =
                CreateBleet.update (CreateBleet.Msg.DisplayModal state.Main.BleeterProfile.Profile) state.CreateBleet

            { state with CreateBleet = create }, Cmd.none
        else
            let main, cmd = Main.update (Main.Msg.UrlChanged url) state.Main

            { state with CurrentUrl = url; Main = main }, (Cmd.map MainMsg cmd)
    | MainMsg msg' ->
        let main, cmd = Main.update msg' state.Main
        { state with Main = main }, (Cmd.map MainMsg cmd)
    | CreateBleetMsg msg' ->
        Router.navigate (state.CurrentUrl |> List.toArray)

        let createBleet = CreateBleet.update msg' state.CreateBleet

        match createBleet.Bleet with
        | None -> { state with CreateBleet = createBleet }, Cmd.none
        | Some bleet ->
            let main, cmd = Main.update ((BleeterProfile.Msg.AddBleet >> Main.Msg.BleeterProfileMsg) bleet) state.Main

            let createBleet = CreateBleet.init ()

            { state with Main = main; CreateBleet = createBleet }, (Cmd.map MainMsg cmd)
    | TrendingMsg msg -> 
        let trending, cmd = Trending.update msg state.Trending
        {state with Trending = trending}, (Cmd.map TrendingMsg cmd)
    | SearchBoxMsg msg ->
        let searchBox = SearchBox.update msg state.SearchBox
        {state with SearchBox = searchBox}, Cmd.none

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
                        Trending.render state.Trending (TrendingMsg >> dispatch) ]
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
    Cmd.batch [
        appOnLoadHeight initial
        appOnResizeHeight initial
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription subscribers
|> Program.run
