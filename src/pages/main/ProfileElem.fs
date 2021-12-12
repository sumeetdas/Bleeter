[<RequireQualifiedAccess>]
module ProfileElem

open Elmish
open Feliz
open Tailwind

type Msg =
    | DataUpdate of Data.State
    | Follow
    | ProfileOptionMsg of Msg EllipsisOption.Msg
    | ReportProfile
    | UrlChanged of string
    | BleetListElemMsg of BleetListElem.Msg

type State =
    {
        Data: Data.State
        ProfileOption: Msg EllipsisOption.State
        Handle: string
        DeletedBleet: Bleet option
        Profile: Profile option
        BleetListElem: BleetListElem.State
        HeightUpdated: bool
        ReportCount: int option
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
    }

let init (data: Data.State) =
    let profileOption: Msg EllipsisOption.State =
        let optionList: Msg EllipsisOption.Option list =
            [
                {
                    Name = "Report Profile"
                    Command = Cmd.ofMsg ReportProfile
                }
            ]

        {
            IsOptionOpen = false
            Coordinates = { X = 0 |> float; Y = 0 |> float }
            Options = optionList
            Size = 20
            CssClasses =
                [
                    tw.border
                    tw.``rounded-full``
                    tw.``border-green-500``
                    tw.``text-green-500``
                    tw.``bleeter-pointer``
                    tw.``hover:bg-green-400``
                    tw.``hover:text-gray-800``
                ]
            Offset = { X = -100.0; Y = 0.0 }
        }

    let bleets =
        match data.Bleets with
        | Resolved (Ok bleets) -> bleets
        | _ -> []

    {
        Data = data
        ProfileOption = profileOption
        Handle = ""
        DeletedBleet = None
        Profile = None
        BleetListElem = BleetListElem.init bleets
        HeightUpdated = false
        ReportCount = None
        NotifMsg = None
        ModalMsg = Modal.DoNothing
    }

let updateBleetListElem (msg: BleetListElem.Msg) (state: State) : State * Msg Cmd =
    let nextBleetListElem, bleetListElemCmd = BleetListElem.update msg state.BleetListElem

    { state with
        BleetListElem = nextBleetListElem
        DeletedBleet = nextBleetListElem.DeletedBleet
        HeightUpdated = nextBleetListElem.HeightUpdated
        NotifMsg = nextBleetListElem.NotifMsg
        ModalMsg = nextBleetListElem.ModalMsg
    },
    Cmd.map BleetListElemMsg bleetListElemCmd

let updateData (state: State) : State * Msg Cmd =
    let state = { state with ModalMsg = Modal.DoNothing }
    // update profile
    let profileOpt =
        match state.Data.Profiles with
        | Resolved (Ok profiles) ->
            profiles
            |> List.tryFind (fun profile -> profile.Handle = state.Handle)
        | _ -> None

    // update bleets
    let bleets =
        match state.Data.Bleets with
        | Resolved (Ok bleets) ->
            bleets
            |> List.filter (fun bleet -> bleet.Handle = state.Handle)
        | _ -> []

    updateBleetListElem (BleetListElem.Msg.DataUpdate bleets) { state with Profile = profileOpt }

let report (state: State) (profile: Profile) (first: string, second: string, modalMsg: Modal.Msg) =
    let nextState =
        match state.ReportCount with
        | None ->
            { state with
                NotifMsg = Some(Notification.msgElem first)
                ReportCount = Some 1
            }
        | Some 1 ->
            { state with
                NotifMsg = Some(Notification.msgElem second)
                ReportCount =
                    state.ReportCount
                    |> Option.bind (fun count -> Some(count + 1))
            }
        | Some _ -> { state with ModalMsg = modalMsg }

    let profileOption, cmd = EllipsisOption.update (EllipsisOption.Close) nextState.ProfileOption
    { nextState with ProfileOption = profileOption }, cmd

let update (msg: Msg) (state: State) : State * Msg Cmd =
    // clear up transient state
    let state =
        { state with
            DeletedBleet = None
            NotifMsg = None
            ModalMsg = Modal.DoNothing
        }

    match msg with
    | DataUpdate data -> updateData { state with Data = data }
    | UrlChanged handle ->
        let nextReportCount = if handle <> state.Handle then None else state.ReportCount
        let nextBleetList, _ = BleetListElem.update (BleetListElem.UrlChanged [ handle ]) state.BleetListElem

        updateData
            { state with
                Handle = handle
                ReportCount = nextReportCount
                BleetListElem = nextBleetList
            }
    | Follow ->
        match state.Profile with
        | Some profile ->
            match profile.IsFollow with
            | None -> state, Cmd.none
            | Some isFollow ->
                let profile = { profile with IsFollow = Some(not isFollow) }

                { state with Profile = Some profile }, Cmd.none
        | None -> state, Cmd.none
    | ProfileOptionMsg msg ->
        let profileOption, cmd = EllipsisOption.update msg state.ProfileOption

        { state with ProfileOption = profileOption }, cmd
    | ReportProfile ->
        match state.Profile with
        | Some profile ->
            if profile.Handle.Contains("Xina") then
                let first = "Operation not permitted!"
                let second = "OPERATION NOT PERMITTED!!!"
                let modalMsg = Modal.ShowCCP [ profile.Handle ]
                report state profile (first, second, modalMsg)
            else
                let first = sprintf "We've reported @%s's profile to Bleeter police." profile.Handle
                let second = "We get it. You are pissed."
                let modalMsg = Modal.ShowMeditation [ profile.Handle ]
                report state profile (first, second, modalMsg)
        | None -> state, Cmd.none
    | BleetListElemMsg msg' -> updateBleetListElem msg' state

let private profileElem
    (profile: Profile)
    (profileOption: Msg EllipsisOption.State)
    (bleetListElem: BleetListElem.State)
    (dispatch: Msg -> unit)
    =
    let followBtn =
        let noShowBtnClasses = [ tw.hidden ]
        let yesFollowClasses = [ tw.``w-28``; tw.``bg-green-500``; tw.``text-white`` ]
        let noFollowClasses = [ tw.``w-20``; tw.``text-green-500`` ]

        let classes =
            [
                [
                    tw.``rounded-full``
                    tw.border
                    tw.``h-10``
                    tw.``mt-3``
                    tw.``ml-2``
                    tw.``border-green-500``
                ]
                (match profile.IsFollow with
                 | None -> noShowBtnClasses
                 | Some isFollow -> if isFollow then yesFollowClasses else noFollowClasses)
            ]
            |> List.concat

        Html.button [
            prop.id "bleeter-follow"
            prop.classes classes
            prop.onClick (fun _ -> dispatch (Follow))
            prop.text (
                match profile.IsFollow with
                | None -> ""
                | Some isFollow -> if isFollow then "Following" else "Follow"
            )
        ]

    let profilePic =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
            ]
            prop.children [
                Html.img [
                    prop.classes [
                        tw.``h-24``
                        tw.``w-24``
                        tw.``rounded-full``
                        tw.``border-4``
                        tw.``border-gray-100``
                        tw.``m-2``
                        tw.flex
                    ]
                    prop.src profile.ProfilePic
                ]
                Html.div [
                    prop.classes [
                        tw.``float-right``
                        tw.flex
                        tw.``flex-grow-2``
                        tw.``flex-row-reverse``
                        tw.``mr-2``
                    ]
                    prop.children [
                        followBtn
                        EllipsisOption.render profileOption (ProfileOptionMsg >> dispatch)
                    ]
                ]
            ]
        ]

    let profileName =
        Html.div [
            prop.classes [ tw.flex; tw.``ml-2`` ]
            prop.children [
                Html.span [ prop.text profile.Name ]
            ]
        ]

    let handle =
        Html.div [
            prop.classes [ tw.flex; tw.``ml-2`` ]
            prop.children [
                Html.span [
                    prop.text ("@" + profile.Handle)
                ]
            ]
        ]

    let otherProfileInfo =
        Html.div [
            prop.classes [ tw.block; tw.``m-2`` ]
            prop.children [
                (match profile.Location with
                 | Some location ->
                     Html.div [
                         prop.classes [ tw.``inline-flex`` ]
                         prop.children [
                             Html.div [
                                 prop.classes [
                                     tw.``flex-1``
                                     tw.``pt-1``
                                 ]
                                 prop.children [
                                     Bleeter.icon "akar-icons:location" "16"
                                 ]
                             ]
                             Html.div [
                                 prop.classes [ tw.``flex-1`` ]
                                 prop.children [
                                     Html.span [ prop.text location ]
                                 ]
                             ]
                         ]
                     ]
                 | None -> Html.div [])
                (match profile.Url with
                 | Some url ->
                     Html.div [
                         prop.classes [
                             tw.``inline-flex``
                             tw.``ml-2``
                         ]
                         prop.children [
                             Html.div [
                                 prop.classes [
                                     tw.``flex-1``
                                     tw.``pt-1``
                                 ]
                                 prop.children [
                                     Bleeter.icon "il:url" "12"
                                 ]
                             ]
                             Html.a [
                                 prop.href url
                                 prop.target "_blank"
                                 prop.classes [
                                     tw.``flex-1``
                                     tw.underline
                                     tw.``text-green-600``
                                 ]
                                 prop.children [
                                     Html.span [
                                         prop.text (Bleeter.getUrl url)
                                     ]
                                 ]
                             ]
                         ]
                     ]
                 | None -> Html.div [])
            ]
        ]

    let followersFollowing =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``m-2``
            ]
            prop.children [
                Html.a [
                    prop.classes [ tw.``select-none`` ]
                    prop.children [
                        Html.span [
                            prop.classes [ tw.``font-bold`` ]
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.classes [ tw.``ml-1`` ]
                            prop.text "Following"
                        ]
                    ]
                ]
                Html.a [
                    prop.classes [
                        tw.``ml-2``
                        tw.``select-none``
                    ]
                    prop.children [
                        Html.span [
                            prop.classes [ tw.``font-bold`` ]
                            prop.text (string profile.Following)
                        ]
                        Html.span [
                            prop.classes [ tw.``ml-1`` ]
                            prop.text "Followers"
                        ]
                    ]
                ]
            ]
        ]

    let latestBleets = MainLayout.heading "Latest Bleets"

    let coreComponents =
        [
            profilePic
            profileName
            handle
            otherProfileInfo
            followersFollowing
            latestBleets
            (BleetListElem.render bleetListElem (BleetListElemMsg >> dispatch))
        ]

    let bannerUrl =
        match profile.Banner with
        | Some url -> Some url
        | None -> Some "/img/bleeter-logo.png"

    MainLayout.elem bannerUrl coreComponents

let render (state: State) (dispatch: Msg -> unit) =
    match state.Profile with
    | Some profile -> profileElem profile state.ProfileOption state.BleetListElem dispatch
    | None -> Html.none
