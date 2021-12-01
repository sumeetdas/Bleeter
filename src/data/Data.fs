[<RequireQualifiedAccess>]
module Data

open Elmish

type State =
    {
        Profiles: Result<Profile list, string> Deferred
        Bleets: Result<Bleet list, string> Deferred
        Distractions: Result<Distraction list, string> Deferred
        MyProfile: Profile option
    }

type Msg =
    | RefreshData
    | AddBleet of Bleet
    | LoadMyProfile
    | LoadProfiles of Result<Profile list, string> AsyncOperationStatus
    | LoadBleets of Result<Bleet list, string> AsyncOperationStatus
    | LoadDistractions of Result<Distraction list, string> AsyncOperationStatus

let init () : State * Cmd<Msg> =
    {
        Profiles = HasNotStartedYet
        Bleets = HasNotStartedYet
        Distractions = HasNotStartedYet
        MyProfile = None
    },
    Cmd.batch [
        Cmd.ofMsg (LoadProfiles Started)
        Cmd.ofMsg (LoadBleets Started)
        Cmd.ofMsg (LoadDistractions Started)
    ]

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | RefreshData -> state, Cmd.none
    | AddBleet bleet ->
        match state.Bleets with
        | Resolved (Ok bleets) ->
            let bleets = [ bleet ] @ bleets
            { state with Bleets = Resolved(Ok bleets) }, Cmd.ofMsg RefreshData
        | _ -> state, Cmd.none
    | LoadMyProfile ->
        match state.Profiles with
        | Resolved (Ok profiles) ->
            let bleeterProfileOpt =
                profiles
                |> List.tryFind (fun profile -> profile.Handle = "bleeter")

            { state with MyProfile = bleeterProfileOpt }, Cmd.none
        | _ -> state, Cmd.none
    | LoadProfiles asyncOpStatus ->
        AsyncOperationStatus.loadData
            state
            asyncOpStatus
            "/data/Profiles.json"
            (fun json -> json |> Profile.decodeListResult)
            (fun result -> LoadProfiles(Finished result))
            (fun state -> { state with Profiles = InProgress })
            (fun (state, profiles) -> { state with Profiles = Resolved profiles })
            (Cmd.ofMsg LoadMyProfile)
    | LoadBleets asyncOpStatus ->
        AsyncOperationStatus.loadData
            state
            asyncOpStatus
            "/data/Bleets.json"
            (fun json -> json |> Bleet.decodeListResult)
            (fun result -> LoadBleets(Finished result))
            (fun state -> { state with Bleets = InProgress })
            (fun (state, bleets) -> { state with Bleets = Resolved bleets })
            Cmd.none
    | LoadDistractions asyncOpStatus ->
        AsyncOperationStatus.loadData
            state
            asyncOpStatus
            "/data/Distractions.json"
            (fun json -> json |> Distraction.decodeListResult)
            (fun result -> LoadDistractions(Finished result))
            (fun state -> { state with Distractions = InProgress })
            (fun (state, distractions) -> { state with Distractions = Resolved distractions })
            Cmd.none
