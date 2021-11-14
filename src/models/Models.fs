// "types-only" module
[<AutoOpen>]
module Models

type Profile = {Name: string; ProfilePic: string; Banner: string; Handle: string; Following: int; Followers: int; Location: string; Url: string}

[<RequireQualifiedAccess>]
module Profile = 
    let init() = {Name = ""; ProfilePic = ""; Banner = ""; Handle = ""; Following = 0; Followers = 0; Location = ""; Url =  ""}

type Bleet = {Name: string; Content: string; ProfilePic: string; Handle: string; Time: string; Rebleets: int; Likes: int; Replies: int}

[<RequireQualifiedAccess>]
module Bleet = 
    let init() = {Name = ""; Content = ""; ProfilePic = ""; Handle = ""; Time = ""; Rebleets = 0; Likes = 0; Replies = 0}
