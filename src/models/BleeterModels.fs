// "types-only" module
[<AutoOpen>]
module BleeterModels

open Thoth.Json

type Coordinates = { X: float; Y: float }

type Profile =
    {
        Name: string
        ProfilePic: string
        Banner: string option
        Handle: string
        Following: int
        Followers: int
        Location: string option
        Url: string option
        IsFollow: bool option
        IsMyProfile: bool
    }

[<RequireQualifiedAccess>]
module Profile =
    let init () =
        {
            Name = ""
            ProfilePic = ""
            Banner = None
            Handle = ""
            Following = 0
            Followers = 0
            Location = None
            Url = None
            IsFollow = None
            IsMyProfile = false
        }

    let decodeListResult (json: string) : Result<Profile list, string> = Decode.Auto.fromString<Profile list> (json)

    let findProfile (handle: string) (profileList: Result<Profile list, string>) : Result<Profile, string> =
        match profileList with
        | Error err -> Error err
        | Ok profileList ->
            let profileOption =
                profileList
                |> List.tryFind (fun profile -> profile.Handle = handle)

            match profileOption with
            | None -> Error(sprintf "profile with handle %s not found" handle)
            | Some profile -> Ok profile

type BleetId = int

type RepliesType = | Boring

type Bleet =
    {
        Id: BleetId
        Name: string
        Content: string
        ProfilePic: string
        Handle: string
        Time: string
        Rebleets: int
        Likes: int
        Replies: int
        RepliesType: RepliesType option
    }

[<RequireQualifiedAccess>]
module Bleet =
    let init () =
        {
            Id = 0
            Name = ""
            Content = ""
            ProfilePic = ""
            Handle = ""
            Time = ""
            Rebleets = 0
            Likes = 0
            Replies = 0
            RepliesType = None
        }

    let decodeListResult (json: string) : Result<Bleet list, string> = Decode.Auto.fromString<Bleet list> (json)

    let getRepliesType (bleet: Bleet) : RepliesType =
        match bleet.RepliesType with
        | None -> Boring
        | Some repliesType -> repliesType

    let findBleets (handle: string) (bleetList: Result<Bleet list, string>) : Result<Bleet list, string> =
        match bleetList with
        | Error err -> Error err
        | Ok bleetList ->
            let bleetList =
                bleetList
                |> List.filter (fun bleet -> bleet.Handle = handle)

            Ok bleetList

type Distraction = { Category: string; Hashtag: string }

[<RequireQualifiedAccess>]
module Distraction =
    let init () = { Category = ""; Hashtag = "" }

    let decodeListResult (json: string) : Result<Distraction list, string> =
        Decode.Auto.fromString<Distraction list> (json)
