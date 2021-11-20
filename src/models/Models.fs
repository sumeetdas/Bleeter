// "types-only" module
[<AutoOpen>]
module Models

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

type Bleet =
    {
        Id: int
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
