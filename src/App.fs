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
        AppHeight: int
    }

// events
type Msg =
    | DataMsg of Data.Msg
    | UrlChanged of string list
    | MainMsg of Main.Msg
    | CreateBleetMsg of CreateBleet.Msg
    | DistractionMsg of Distraction.Msg
    | SearchBoxMsg of SearchBox.Msg
    | UpdateHeight of int

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
        AppHeight = 500
    },
    Cmd.batch [
        (Cmd.map MainMsg mainCmd)
        (Cmd.map DataMsg dataCmd)
    ]

let changeUrl (url: string list, state: State) =
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

let getWindowHeight() = 
    let scrollHeight =
        (document.getElementById "elmish-app")
            .scrollHeight
        |> int

    let windowHeight = window.innerHeight |> int

    if scrollHeight > windowHeight then
        scrollHeight
    else
        windowHeight

let updateData (state: State) =
    if state.Data.DoSyncData then
        let nextData, dataCmd = Data.update (Data.Msg.DoneSyncData) state.Data
        let nextMain, mainCmd = Main.update (Main.Msg.DataUpdate state.Data) state.Main

        let nextDistraction, distractionCmd =
            Distraction.update (Distraction.Msg.DataUpdate state.Data) state.Distraction

        let resizeCmd (dispatch: Msg -> unit) = 
            let delayedHeightCheck = 
                async {
                    do! Async.Sleep 1000
                    let finalHeight = getWindowHeight()
                    printf "Meow %d" finalHeight
                    dispatch (UpdateHeight finalHeight)
                }

            Async.StartImmediate delayedHeightCheck

        { state with
            Data = nextData
            Main = nextMain
            Distraction = nextDistraction
        },
        Cmd.batch [
            Cmd.map MainMsg mainCmd
            Cmd.map DistractionMsg distractionCmd
            Cmd.map DataMsg dataCmd
            Cmd.ofSub resizeCmd
        ]
    else
        { state with Data = state.Data }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | UpdateHeight height -> 
        let nextMain, mainCmd = Main.update (Main.Msg.AppHeight height) state.Main
        { state with Main = nextMain; AppHeight = height }, Cmd.map MainMsg mainCmd
    | DataMsg msg' ->
        let data, dataCmd = Data.update msg' state.Data
        let nextState = { state with Data = data }
        let nextState, newCmd = updateData nextState

        nextState,
        Cmd.batch [
            (Cmd.map DataMsg dataCmd)
            newCmd
        ]
    | UrlChanged url -> changeUrl (url, state)
    | MainMsg msg' ->
        let nextMain, mainCmd = Main.update msg' state.Main
        let nextState = { state with Main = nextMain }

        match nextMain.DeletedBleet with
        | Some bleet ->
            let nextData, dataCmd = Data.update (Data.Msg.DeleteBleet bleet) nextState.Data
            let nextState = { nextState with Data = nextData }
            let nextState, updateDataCmd = updateData nextState

            nextState,
            Cmd.batch [
                Cmd.map MainMsg mainCmd
                Cmd.map DataMsg dataCmd
                updateDataCmd
            ]
        | None -> nextState, Cmd.map MainMsg mainCmd
    | CreateBleetMsg msg' ->
        let createBleet, createBleetCmd = CreateBleet.update msg' state.CreateBleet
        let nextState = { state with CreateBleet = createBleet }

        match nextState.CreateBleet.Bleet with
        | None -> nextState, Cmd.map CreateBleetMsg createBleetCmd
        | Some bleet ->
            let nextCreateBleet, _ = CreateBleet.update CreateBleet.Msg.CloseModal nextState.CreateBleet
            let nextState = { nextState with CreateBleet = nextCreateBleet }
            let nextData, dataCmd = Data.update (Data.Msg.AddBleet bleet) nextState.Data
            let nextState = { nextState with Data = nextData }
            let nextState, updateDataCmd = updateData nextState
            let nextState, urlChangeCmd = changeUrl (createBleet.PreviousUrl, nextState)

            nextState,
            (Cmd.batch [
                Cmd.map CreateBleetMsg createBleetCmd
                Cmd.map DataMsg dataCmd
                updateDataCmd
                urlChangeCmd
             ])
    | DistractionMsg msg ->
        let distraction, cmd = Distraction.update msg state.Distraction
        { state with Distraction = distraction }, (Cmd.map DistractionMsg cmd)
    | SearchBoxMsg msg ->
        let nextSearchBox = SearchBox.update msg state.SearchBox
        let nextState = { state with SearchBox = nextSearchBox }

        if nextSearchBox.DoSearch then
            Router.navigate ("search", nextSearchBox.Content)
            let nextSearchBox = SearchBox.update SearchBox.Msg.Clear nextState.SearchBox
            { nextState with SearchBox = nextSearchBox }, Cmd.none
        else
            nextState, Cmd.none

let render (state: State) (dispatch: Msg -> Unit) =
    let page =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``min-h-full``
                tw.``h-full``
            ]
            prop.style [ style.height state.AppHeight ]
            prop.children [
                Menu.menuHtml state.AppHeight

                (Main.render state.Main (MainMsg >> dispatch))

                Html.div [
                    prop.classes [ tw.``flex-grow-1`` ]
                    prop.style [ style.height state.AppHeight ]
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

let appOnLoadHeight _ =
    let sub dispatch =
        window.addEventListener (
            "load",
            fun _ ->
                let finalHeight = getWindowHeight()
                dispatch (UpdateHeight finalHeight)
        )

    Cmd.ofSub sub

let appOnResizeHeight _ =
    let sub dispatch =
        window.addEventListener (
            "resize",
            fun _ ->
                let finalHeight = getWindowHeight()
                dispatch (UpdateHeight finalHeight)
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
