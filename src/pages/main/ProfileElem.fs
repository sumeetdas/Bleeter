[<RequireQualifiedAccess>]
module ProfileElem

open Elmish
open Feliz
open Tailwind
open Fable.SimpleHttp

type Msg =
    | AddBleet of Bleet
    | Follow
    | ProfileOptionMsg of Msg EllipsisOption.Msg
    | ReportProfile
    | BleetElemMsg of BleetId * BleetElem.Msg
    | LoadProfile of Result<Profile, string> AsyncOperationStatus
    | LoadBleets of Result<Bleet list, string> AsyncOperationStatus
    | UrlChanged of string

type State =
    {
        Data: Data.State
        BleetElems: Result<BleetElem.State list, string> Deferred
        Profile: Result<Profile, string> Deferred
        ProfileOption: Msg EllipsisOption.State
        Handle: string
        DeleteBleet: Bleet option
    }

let init (data: Data.State) =
    let profileOption: Msg EllipsisOption.State =
        let optionList: Msg EllipsisOption.Option list =
            [
                {
                    Name = "ReportProfile"
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

    {
        Data = data
        BleetElems = HasNotStartedYet
        Profile = HasNotStartedYet
        ProfileOption = profileOption
        Handle = "Bleeter"
        DeleteBleet = None
    }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | UrlChanged handle ->
        let newState = { state with Handle = handle }

        newState,
        Cmd.batch [
            Cmd.ofMsg (LoadProfile Started)
            Cmd.ofMsg (LoadBleets Started)
        ]
    | AddBleet bleet ->
        match state.BleetElems with
        | Resolved (Ok bleetElems) ->
            let bleetElems = (bleet |> BleetElem.init) :: bleetElems

            { state with BleetElems = Resolved(Ok bleetElems) }, Cmd.none
        | _ -> state, Cmd.none
    | Follow ->
        match state.Profile with
        | Resolved (Ok profile) ->
            match profile.IsFollow with
            | None -> state, Cmd.none
            | Some isFollow ->
                let profile = { profile with IsFollow = Some(not isFollow) }

                { state with Profile = Resolved(Ok profile) }, Cmd.none
        | _ -> state, Cmd.none
    | ProfileOptionMsg msg ->
        let profileOption, cmd = EllipsisOption.update msg state.ProfileOption

        { state with ProfileOption = profileOption }, cmd
    | ReportProfile ->
        printf "ReportProfile"
        state, Cmd.none
    | BleetElemMsg (id: int, msg') ->
        match state.BleetElems with
        | Resolved (Ok bleetElems) ->
            let bleetElemOpt =
                bleetElems
                |> List.tryFind (fun bleetElem -> bleetElem.Bleet.Id = id)

            match bleetElemOpt with
            | Some bleetElem ->
                let nextBleetElem, bleetElemCmd = BleetElem.update msg' bleetElem
                let bleetElemCmd = Cmd.map (fun msg -> BleetElemMsg(id, msg)) bleetElemCmd

                let bleetElems =
                    bleetElems
                    |> List.updateAt (fun bleetElem -> bleetElem.Bleet.Id = id) nextBleetElem

                if nextBleetElem.IsDeleted then
                    { state with
                        BleetElems = Resolved(Ok bleetElems)
                        DeleteBleet = Some bleetElem.Bleet
                    },
                    bleetElemCmd
                else
                    { state with BleetElems = Resolved(Ok bleetElems) }, bleetElemCmd
            | None -> state, Cmd.none
        | _ -> state, Cmd.none
    | LoadProfile asyncOpStatus ->
        match asyncOpStatus with
        | Started ->
            let nextState = { state with Profile = InProgress }

            let loadProfileCmd =
                async {
                    let! (statusCode, profileList) = "/data/Profiles.json" |> Http.get

                    if statusCode = 200 then
                        return
                            LoadProfile(
                                Finished(
                                    profileList
                                    |> Profile.decodeListResult
                                    |> Profile.findProfile nextState.Handle
                                )
                            )
                    else
                        return LoadProfile(Finished(Error "error while fetching profile"))
                }

            nextState, Cmd.fromAsync loadProfileCmd

        | Finished result ->
            let nextState = { state with Profile = Resolved result }
            nextState, Cmd.none
    | LoadBleets asyncOpStatus ->
        match asyncOpStatus with
        | Started ->
            let nextState = { state with Profile = InProgress }

            let loadBleetsCmd =
                async {
                    let! (statusCode, bleetList) = "/data/Bleets.json" |> Http.get

                    if statusCode = 200 then
                        return
                            LoadBleets(
                                Finished(
                                    bleetList
                                    |> Bleet.decodeListResult
                                    |> Bleet.findBleets nextState.Handle
                                )
                            )
                    else
                        return LoadBleets(Finished(Error "error while fetching profile"))
                }

            nextState, Cmd.fromAsync loadBleetsCmd
        | Finished result ->
            match result with
            | Ok bleets ->
                let bleetElems = bleets |> List.map BleetElem.init

                let nextState = { state with BleetElems = Resolved(Ok bleetElems) }
                nextState, Cmd.none
            | Error err ->
                printf "%A" err
                state, Cmd.none

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
    (bleetElems: BleetElem.State list)
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
            let bleetList =
                bleetElems
                |> List.map
                    (fun bleetElem ->
                        BleetElem.render
                            bleetElem
                            ((fun msg -> BleetElemMsg(bleetElem.Bleet.Id, msg))
                             >> dispatch))

            Html.div [ prop.children bleetList ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    match state.Profile with
    | Resolved (Ok profile) ->
        match state.BleetElems with
        | Resolved (Ok bleetElems) -> renderedElem profile state.ProfileOption bleetElems dispatch
        | Resolved (Error err) ->
            printf "%A" err
            Html.none
        | _ -> Html.none
    | Resolved (Error err) ->
        printf "%A" err
        Html.none
    | _ -> Html.none
