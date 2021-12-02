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
                    tw.``cursor-pointer``
                    tw.``hover:bg-green-400``
                    tw.``hover:text-gray-800``
                ]
            Offset = { X = -90.0; Y = 30.0 }
        }

    let bleets =
        match data.Bleets with
        | Resolved (Ok bleets) -> bleets
        | _ -> []

    {
        Data = data
        ProfileOption = profileOption
        Handle = "Bleeter"
        DeletedBleet = None
        Profile = None
        BleetListElem = BleetListElem.init bleets
    }

let updateBleetListElem (msg: BleetListElem.Msg) (state: State) : State * Msg Cmd =
    let nextBleetListElem, bleetListElemCmd = BleetListElem.update msg state.BleetListElem
    { state with BleetListElem = nextBleetListElem }, Cmd.map BleetListElemMsg bleetListElemCmd

let updateData (state: State) : State * Msg Cmd =
    // update profile
    let profileOpt =
        match state.Data.Profiles with
        | Resolved (Ok profiles) ->
            profiles
            |> List.tryFind (fun profile -> profile.Handle = "bleeter")
        | _ -> None

    // update bleets
    let bleets =
        match state.Data.Bleets with
        | Resolved (Ok bleets) ->
            bleets
            |> List.filter (fun bleet -> bleet.Handle = "bleeter")
        | _ -> []

    updateBleetListElem (BleetListElem.Msg.DataUpdate bleets) { state with Profile = profileOpt }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DataUpdate data -> updateData { state with Data = data }
    | UrlChanged handle ->
        let newState = { state with Handle = handle }
        newState, Cmd.none
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
        printf "Report Profile"
        state, Cmd.none
    | BleetListElemMsg msg' -> updateBleetListElem msg' state

let bleetProfileElem (profile: Profile) (profileOption: Msg EllipsisOption.State) (dispatch: Msg -> unit) =
    let followBtn =
        let noShowBtnClasses = [ tw.hidden ]
        let yesFollowClasses = [ tw.``w-36`` ]
        let noFollowClasses = [ tw.``w-20`` ]

        let classes =
            [
                [
                    tw.``rounded-full``
                    tw.border
                    tw.``h-10``
                    tw.``mt-3``
                    tw.``ml-2``
                    tw.``border-green-500``
                    tw.``text-green-500``
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

    Html.div [
        (match profile.Banner with
         | Some url ->
             Html.img [
                 prop.classes [
                     tw.``w-full``
                     tw.``bg-cover``
                 ]
                 prop.src url
             ]
         | None ->
             Html.div [
                 prop.classes [
                     tw.``w-full``
                     tw.``border-none``
                     tw.``bg-transparent``
                 ]
             ])
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
        Html.div [
            prop.classes [ tw.flex; tw.``ml-2`` ]
            prop.children [
                Html.span [ prop.text profile.Name ]
            ]
        ]
        Html.div [
            prop.classes [ tw.flex; tw.``ml-2`` ]
            prop.children [
                Html.span [
                    prop.text ("@" + profile.Handle)
                ]
            ]
        ]
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
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-row``
                tw.``m-2``
            ]
            prop.children [
                Html.a [
                    prop.href "#"
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
                    prop.classes [ tw.``ml-2`` ]
                    prop.href "#"
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
    ]

let renderedElem
    (profile: Profile)
    (profileOption: Msg EllipsisOption.State)
    (bleetListElem: BleetListElem.State)
    (dispatch: Msg -> unit)
    : ReactElement =
    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
        ]
        prop.children [
            bleetProfileElem profile profileOption dispatch

            Html.div [
                prop.classes [
                    tw.``text-2xl``
                    tw.``h-12``
                    tw.``border-b``
                    tw.``border-gray-300``
                    tw.``text-green-600``
                ]
                prop.children [
                    Html.span [
                        prop.classes [ tw.``m-6`` ]
                        prop.text "Latest Bleets"
                    ]
                ]
            ]
            BleetListElem.render bleetListElem (BleetListElemMsg >> dispatch)
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    match state.Profile with
    | Some profile -> renderedElem profile state.ProfileOption state.BleetListElem dispatch
    | None -> Html.none
