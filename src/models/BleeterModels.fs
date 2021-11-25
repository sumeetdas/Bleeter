// "types-only" module
[<AutoOpen>]
module BleeterModels

open Thoth.Json

type Coordinates = { X: float; Y: float }

type Profile =
    {
        Name: string
        ProfilePic: string
        Banner: string
        Handle: string
        Following: int
        Followers: int
        Location: string
        Url: string
        IsFollow: bool option
    }

[<RequireQualifiedAccess>]
module Profile =
    let init () =
        {
            Name = ""
            ProfilePic = ""
            Banner = ""
            Handle = ""
            Following = 0
            Followers = 0
            Location = ""
            Url = ""
            IsFollow = None
        }

    let decodeResult (json: string) : Result<Profile, string> = Decode.Auto.fromString<Profile> (json)

type BleetId = int

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
        }

    let decodeListResult (json: string) : Result<Bleet list, string> = Decode.Auto.fromString<Bleet list> (json)
