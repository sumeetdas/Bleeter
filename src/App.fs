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
        DistractionElemList: DistractionElemList.State
        SearchBox: SearchBox.State
        AppHeight: int option
        Notification: Notification.State
        Modal: Modal.State
        ScreenSize: ScreenSize
        MobileMenu: MobileMenu.State
    }

// events
type Msg =
    | DataMsg of Data.Msg
    | UrlChanged of string list
    | MainMsg of Main.Msg
    | DistractionElemListMsg of DistractionElemList.Msg
    | SearchBoxMsg of SearchBox.Msg
    | NotificationMsg of Notification.Msg
    | ModalMsg of Modal.Msg
    | ScreenSizeUpdated of int option * ScreenSize option
    | MobileMenuMsg of MobileMenu.Msg

// need parentheses for indicating that init is a function
let init () =
    let currentUrl = Router.currentUrl ()
    let data, dataCmd = Data.init ()
    let main, mainCmd = Main.init currentUrl data

    {
        Data = data
        CurrentUrl = currentUrl
        Main = main
        DistractionElemList = DistractionElemList.init data
        SearchBox = SearchBox.init()
        AppHeight = None
        Notification = Notification.init()
        Modal = Modal.init data
        ScreenSize = Mobile
        MobileMenu = MobileMenu.init()
    },
    Cmd.batch [
        (Cmd.map MainMsg mainCmd)
        (Cmd.map DataMsg dataCmd)
    ]

let scrollToTop () =
    let delayedScrollToTop =
        async {
            // do! Async.Sleep 250
            window.scrollTo (0.0, 0.0)
        }

    Async.StartImmediate delayedScrollToTop

let getWindowHeight () =
    let scrollHeight =
        (document.getElementById "elmish-app")
            .scrollHeight
        |> int

    let windowHeight = window.innerHeight |> int

    if scrollHeight > windowHeight then
        scrollHeight
    else
        windowHeight

let resizeCmd (dispatch: Msg -> unit) =
    dispatch (ScreenSizeUpdated (None, None))
    let delayedHeightCheck =
        async {
            do! Async.Sleep 200
            let finalHeight = getWindowHeight ()
            let width = Bleeter.getWindowWidth ()
            dispatch (ScreenSizeUpdated (Some finalHeight, Some (width |> ScreenSize.getSize)))
        }

    [ delayedHeightCheck; delayedHeightCheck ]
    |> Async.Sequential
    |> Async.Ignore
    |> Async.StartImmediate

let resetHeight state = 
    let main, _ = Main.update (Main.AppHeight None) state.Main
    { state with Main = main; AppHeight = None }

let changeUrl (url: string list, state: State) =
    let state = { state with Modal = Modal.init state.Data }

    match url with
    | [ "create"; "bleet" ] ->
        let nextUrl =
            if state.CurrentUrl = [ "create"; "bleet" ] then
                []
            else
                state.CurrentUrl

        if Bleeter.isMobile () then
            let main, mainCmd = Main.update (Main.UrlChanged([ "mobile" ] @ url)) state.Main
            { state with Main = main; CurrentUrl = nextUrl }, Cmd.map MainMsg mainCmd
        else
            let modal, modalCmd = Modal.update (Modal.ShowCreateBleet nextUrl) state.Modal
            { state with Modal = modal; CurrentUrl = nextUrl }, Cmd.map ModalMsg modalCmd
    | _ ->
        let state = resetHeight state
        let main, mainCmd = Main.update (Main.UrlChanged url) state.Main

        let distraction, distractionCmd =
            DistractionElemList.update (DistractionElemList.Msg.UrlChanged url) state.DistractionElemList

        let modal, modalCmd = Modal.update (Modal.Msg.UrlChanged url) state.Modal
        scrollToTop ()

        { state with
            CurrentUrl = url
            Main = main
            DistractionElemList = distraction
            Modal = modal
        },
        Cmd.batch [
            Cmd.map MainMsg mainCmd
            Cmd.map DistractionElemListMsg distractionCmd
            Cmd.map ModalMsg modalCmd
            Cmd.ofSub resizeCmd
        ]

let updateData (state: State) =
    if state.Data.DoSyncData then
        let nextData, dataCmd = Data.update (Data.DoneSyncData) state.Data
        let nextMain, mainCmd = Main.update (Main.DataUpdate state.Data) state.Main
        let nextModal, modalCmd = Modal.update (Modal.DataUpdated state.Data) state.Modal

        let nextDistraction, distractionCmd =
            DistractionElemList.update (DistractionElemList.Msg.DataUpdate state.Data) state.DistractionElemList

        { state with
            Data = nextData
            Main = nextMain
            DistractionElemList = nextDistraction
            Modal = nextModal
        },
        Cmd.batch [
            Cmd.map MainMsg mainCmd
            Cmd.map DistractionElemListMsg distractionCmd
            Cmd.map DataMsg dataCmd
            Cmd.map ModalMsg modalCmd
            Cmd.ofSub resizeCmd
        ]
    else
        { state with Data = state.Data }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | ScreenSizeUpdated (heightOpt, screenSizeOpt) -> 
        let state = 
            let nextMain, _ = Main.update (Main.Msg.AppHeight heightOpt) state.Main
            { state with Main = nextMain; AppHeight = heightOpt }
    
        match screenSizeOpt with 
        | Some screenSize -> { state with ScreenSize = screenSize }, Cmd.none
        | None -> state, Cmd.none
    | DataMsg msg' ->
        let data, dataCmd = Data.update msg' state.Data
        let nextState = { state with Data = data }
        let nextState, newCmd = updateData nextState

        nextState,
        Cmd.batch [
            (Cmd.map DataMsg dataCmd)
            newCmd
        ]
    | UrlChanged url ->
        let state, cmd = changeUrl (url, state)
        state, cmd
    | MainMsg msg' ->
        let nextMain, mainCmd = Main.update msg' state.Main
        let nextState = { state with Main = nextMain }

        let resizeCmd = if nextMain.HeightUpdated then Cmd.ofSub resizeCmd else Cmd.none

        let nextState, deleteBleetCmd =
            match nextMain.DeletedBleet with
            | Some bleet ->
                let nextData, dataCmd = Data.update (Data.Msg.DeleteBleet bleet) nextState.Data
                let nextState = { nextState with Data = nextData }
                let nextState, updateDataCmd = updateData nextState

                nextState,
                Cmd.batch [
                    Cmd.map DataMsg dataCmd
                    updateDataCmd
                ]
            | None -> nextState, Cmd.none

        let nextState, addBleetCmd =
            match nextMain.AddBleet with
            | Some bleet ->
                let nextData, dataCmd = Data.update (Data.Msg.AddBleet bleet) nextState.Data
                let nextState = { nextState with Data = nextData }
                let nextState, updateDataCmd = updateData nextState

                nextState,
                Cmd.batch [
                    Cmd.map DataMsg dataCmd
                    updateDataCmd
                ]
            | None -> nextState, Cmd.none

        let nextState, notifCmd =
            let nextNotif, notifCmd = Notification.update (Notification.Show nextMain.NotifMsg) state.Notification
            { nextState with Notification = nextNotif }, Cmd.map NotificationMsg notifCmd

        let nextState, modalCmd =
            let nextModal, modalCmd = Modal.update nextMain.ModalMsg state.Modal
            { nextState with Modal = nextModal }, Cmd.map ModalMsg modalCmd

        nextState,
        Cmd.batch [
            Cmd.map MainMsg mainCmd
            resizeCmd
            deleteBleetCmd
            addBleetCmd
            notifCmd
            modalCmd
        ]
    | DistractionElemListMsg msg ->
        let distractionElemList, cmd = DistractionElemList.update msg state.DistractionElemList

        let nextState = { state with DistractionElemList = distractionElemList }

        let nextState, notifCmd =
            let nextNotif, notifCmd =
                Notification.update (Notification.Show distractionElemList.NotifMsg) state.Notification

            { nextState with Notification = nextNotif }, Cmd.map NotificationMsg notifCmd

        let nextState, modalCmd =
            let nextModal, modalCmd = Modal.update distractionElemList.ModalMsg state.Modal
            { nextState with Modal = nextModal }, Cmd.map ModalMsg modalCmd

        nextState,
        Cmd.batch [
            (Cmd.map DistractionElemListMsg cmd)
            notifCmd
            modalCmd
        ]
    | SearchBoxMsg msg ->
        let nextSearchBox = SearchBox.update msg state.SearchBox
        let nextState = { state with SearchBox = nextSearchBox }

        if nextSearchBox.DoSearch then
            Router.navigate ("search", nextSearchBox.Content)
            let nextSearchBox = SearchBox.update SearchBox.Msg.Clear nextState.SearchBox
            { nextState with SearchBox = nextSearchBox }, Cmd.none
        else
            nextState, Cmd.none
    | NotificationMsg msg' ->
        let nextNotif, notifCmd = Notification.update msg' state.Notification
        { state with Notification = nextNotif }, Cmd.map NotificationMsg notifCmd
    | ModalMsg msg' ->
        let nextModal, modalCmd = Modal.update msg' state.Modal
        let nextState = { state with Modal = nextModal }

        match nextState.Modal.NewBleet with
        | None -> nextState, Cmd.map ModalMsg modalCmd
        | Some bleet ->
            let nextModal, _ = Modal.update Modal.Close nextState.Modal
            let nextState = { nextState with Modal = nextModal }
            let nextData, dataCmd = Data.update (Data.Msg.AddBleet bleet) nextState.Data
            let nextState = { nextState with Data = nextData }
            let nextState, updateDataCmd = updateData nextState
            let nextState, urlChangeCmd = changeUrl (nextModal.PreviousUrl, nextState)

            nextState,
            (Cmd.batch [
                Cmd.map ModalMsg modalCmd
                Cmd.map DataMsg dataCmd
                updateDataCmd
                urlChangeCmd
             ])
    | MobileMenuMsg msg' -> 
        let menu = MobileMenu.update msg' state.MobileMenu
        { state with MobileMenu = menu }, Cmd.none

let mobileElem (state: State) (dispatch: Msg -> unit) = 
    Html.div [
        Html.div [
            prop.classes [
                tw.``fixed``
                tw.``bg-bleeter-blue``
                tw.``flex``
                tw.``flex-row``
                tw.``flex-grow-1``
                tw.``w-full``
                tw.``text-gray-100``
                tw.``bleeter-pointer``
                tw.``pl-2``
            ]
            prop.onClick (fun _ -> MobileMenu.Display |> MobileMenuMsg |> dispatch)
            prop.children [
                Bleeter.icon "ci:hamburger" "32"
            ]
        ]
        Html.div [
            prop.classes [
                tw.``fixed`` 
                tw.``top-0`` 
                tw.``right-0``
                tw.``mr-2``
                tw.``mt-8``
                (if Router.currentUrl() = [ "create"; "bleet" ] then tw.``hidden`` else tw.``block``)
            ]
            prop.children [
                Menu.bleetButton
            ]
        ]
        MobileMenu.render state.MobileMenu (MobileMenuMsg >> dispatch)
    ]

let render (state: State) (dispatch: Msg -> Unit) =
    let heightStyle = 
        match state.AppHeight with 
        | Some height -> [ style.height height ]
        | None -> []

    let page =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``min-h-full``
                tw.``h-full``
            ]
            prop.style heightStyle
            prop.children [
                (
                    if state.ScreenSize |> ScreenSize.isMobile
                    then mobileElem state dispatch
                    else Html.none
                )

                Menu.menuHtml state.AppHeight state.CurrentUrl

                (Main.render state.Main (MainMsg >> dispatch))

                Html.div [
                    prop.classes [
                        tw.``flex-grow-1``
                        tw.hidden
                        tw.``lg:flex``
                        tw.``lg:flex-col``
                    ]
                    prop.style heightStyle
                    prop.children [
                        SearchBox.render state.SearchBox (SearchBoxMsg >> dispatch)
                        DistractionElemList.render state.DistractionElemList (DistractionElemListMsg >> dispatch)
                    ]
                ]
                (Modal.render state.Modal (ModalMsg >> dispatch))

                (Notification.render state.Notification (NotificationMsg >> dispatch))
            ]
        ]

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)
        router.children page
    ]

let appOnLoadHeight _ =
    let sub dispatch = window.addEventListener ("load", fun _ -> dispatch |> resizeCmd)

    Cmd.ofSub sub

let appOnResizeHeight _ =
    let sub dispatch = window.addEventListener ("resize", fun _ -> dispatch |> resizeCmd)

    Cmd.ofSub sub

let appOnOrientationChange _ =
    // listening to 'change' event in screen.orientation does not work
    // in Firefox. Commenting it for now 
    // let sub dispatch = window.screen.orientation.addEventListener ("change", (
    //     fun _ -> 
    //         printf "appOnOrientationChange"
    //         sizeUpdate dispatch))

    let sub dispatch = window.addEventListener ("orientationchange", fun _ -> dispatch |> resizeCmd)

    Cmd.batch [
        Cmd.ofSub sub
    ]
    
// Subscriptions for DOM events doesn't work.
// GitHub Issue: https://github.com/elmish/elmish/issues/229
// let followClick initial =
//     let sub dispatch = window.addEventListener("load", fun _ ->
//         let followButton = document.getElementById "bleeter-follow"
//         followButton.addEventListener("onClick", fun _ ->
//             true |> ignore
//         )
//     )
//     Cmd.ofSub sub

let subscribers initial =
    let currentUrl = Router.currentUrl ()

    Cmd.batch [
        appOnLoadHeight initial
        appOnResizeHeight initial
        appOnOrientationChange initial
        Cmd.ofMsg (UrlChanged(if currentUrl.Length = 0 then [ "home" ] else currentUrl))
    ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription subscribers
|> Program.run
