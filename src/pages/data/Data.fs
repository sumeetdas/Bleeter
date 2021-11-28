[<RequireQualifiedAccess>]
module Data

open Elmish
open Fable.SimpleHttp

type State =
    {
        Profiles: Result<Profile list, string> Deferred
        Bleets: Result<Bleet list, string> Deferred
        Distractions: Result<Distraction list, string> Deferred
    }

type Msg =
    | LoadProfiles of Result<Profile list, string> AsyncOperationStatus
    | LoadBleets of Result<Bleet list, string> AsyncOperationStatus
    | LoadDistractions of Result<Distraction list, string> AsyncOperationStatus

let init () : State * Cmd<Msg> =
    {
        Profiles = HasNotStartedYet
        Bleets = HasNotStartedYet
        Distractions = HasNotStartedYet
    },
    Cmd.batch [
        Cmd.ofMsg (LoadProfiles Started)
        Cmd.ofMsg (LoadBleets Started)
        Cmd.ofMsg (LoadDistractions Started)
    ]

let loadData
    (state: State)
    (asyncOpStatus: Result<'b list, string> AsyncOperationStatus)
    (jsonUrl: string)
    (jsonToList: string -> Result<'b list, string>)
    (resultToMsg: Result<'b list, string> -> Msg)
    (toInProgressState: State -> State)
    (toResolvedState: State * Result<'b list, string> -> State)
    : State * Cmd<Msg> =

    match asyncOpStatus with
    | Started ->
        let nextState = toInProgressState state

        let loadCmd =
            async {
                let! (statusCode, json) = jsonUrl |> Http.get

                if statusCode = 200 then
                    return json |> jsonToList |> resultToMsg
                else
                    return Error "error while fetching profile" |> resultToMsg
            }

        nextState, Cmd.fromAsync loadCmd
    | Finished result ->
        let nextState = toResolvedState (state, result)
        nextState, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | LoadProfiles asyncOpStatus ->
        loadData
            state
            asyncOpStatus
            "/data/Profiles.json"
            (fun json -> json |> Profile.decodeListResult)
            (fun result -> LoadProfiles(Finished result))
            (fun state -> { state with Profiles = InProgress })
            (fun (state, profiles) -> { state with Profiles = Resolved profiles })
    | LoadBleets asyncOpStatus ->
        loadData
            state
            asyncOpStatus
            "/data/Bleets.json"
            (fun json -> json |> Bleet.decodeListResult)
            (fun result -> LoadBleets(Finished result))
            (fun state -> { state with Bleets = InProgress })
            (fun (state, bleets) -> { state with Bleets = Resolved bleets })
    | LoadDistractions asyncOpStatus ->
        loadData
            state
            asyncOpStatus
            "/data/Distractions.json"
            (fun json -> json |> Distraction.decodeListResult)
            (fun result -> LoadDistractions(Finished result))
            (fun state -> { state with Distractions = InProgress })
            (fun (state, distractions) -> { state with Distractions = Resolved distractions })
