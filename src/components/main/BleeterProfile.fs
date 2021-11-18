[<RequireQualifiedAccess>]
module BleeterProfile

open Feliz
open Tailwind
open Browser
// for `?` operator
open Fable.Core.JsInterop

type ProfileOption = { IsProfileOptionOpen: bool; Coordinates: Coordinates  }

type State =
    { Bleets: Bleet list
      Profile: Profile
      ProfileOption: ProfileOption }

type Msg = 
    | AddBleet of Bleet
    | Follow
    | CloseProfileOptions
    | OpenProfileOptions of Coordinates

let init () =
    let bleets: Bleet list =
        [ { Name = "Bleeter Boi"
            Content = "Hello Bleeter!"
            ProfilePic = "/bleeter_profile_pic.png"
            Handle = "BleeterBoi"
            Time = ""
            Rebleets = 123
            Likes = 3000
            Replies = 0 }
          { Name = "Sheeple"
            Content = "We the Sheeple!"
            ProfilePic = "/bleeter_profile_pic.png"
            Handle = "Sheeple"
            Time = ""
            Rebleets = 1230
            Likes = 40000
            Replies = 1 }
          { Name = "John Xina"
            Content = "“The enemy can’t hit what they can’t see.”- John Xina, the art of war"
            ProfilePic = "/john_xina.png"
            Handle = "JohnXina"
            Time = ""
            Rebleets = 1230
            Likes = 40000
            Replies = 1 } ]

    let profile: Profile =
        { Name = "Bleeter"
          ProfilePic = "/bleeter_profile_pic.png"
          Banner = "/bleeter_banner.jpg"
          Handle = "bleeter"
          Following = 30
          Followers = 24
          Url = "https://sumeetdas.me/bleeter"
          Location = "Hill"
          IsFollow = Some false }

    let profileOption = {IsProfileOptionOpen = false; Coordinates = {X = 0 |> float; Y = 0 |> float}}
    { Bleets = bleets; Profile = profile; ProfileOption = profileOption }

let update (msg: Msg) (state: State) : State =
    match msg with
    | AddBleet bleet ->
        let bleets = bleet :: state.Bleets
        { state with Bleets = bleets }
    | Follow -> 
        match state.Profile.IsFollow with
        | None -> state
        | Some isFollow -> 
            let profile = {state.Profile with IsFollow = Some (not isFollow)} 
            {state with Profile = profile}
    | CloseProfileOptions -> {state with ProfileOption = {state.ProfileOption with IsProfileOptionOpen = false}}
    | OpenProfileOptions coordinates -> 
        printf "%A" coordinates
        {state with ProfileOption = {IsProfileOptionOpen = true; Coordinates = coordinates}}

let bleetElem (bleet: Bleet) =
    Html.article [ prop.classes [ tw.``hover:bg-gray-300``
                                  tw.flex
                                  tw.``flex-row``
                                  tw.``p-4``
                                  tw.``pb-0``
                                  tw.``border-b``
                                  tw.``border-gray-300`` ]
                   prop.children [ Html.div [ prop.classes [ tw.flex
                                                             tw.``flex-grow-0``
                                                             tw.``w-12`` ]
                                              prop.children [ Html.img [ prop.classes [ tw.``h-12``
                                                                                        tw.``w-12``
                                                                                        tw.``rounded-full``
                                                                                        tw.``border-2``
                                                                                        tw.``border-gray-100`` ]
                                                                         prop.src bleet.ProfilePic ] ] ]
                                   Html.div [ prop.classes [ tw.flex
                                                             tw.``flex-col``
                                                             tw.``flex-grow-1``
                                                             tw.``pl-2`` ]
                                              prop.children [ Html.div [ prop.classes [ tw.flex
                                                                                        tw.``flex-row`` ]
                                                                         prop.children [ Html.div [ prop.classes [ tw.flex
                                                                                                                   tw.``flex-grow-1`` ]
                                                                                                    prop.children [ Html.span [ prop.classes [ tw.``hover:underline`` ]
                                                                                                                                prop.text
                                                                                                                                    bleet.Name ]
                                                                                                                    Html.span [ prop.classes [ tw.``ml-1`` ]
                                                                                                                                prop.text (
                                                                                                                                    "@"
                                                                                                                                    + bleet.Handle
                                                                                                                                ) ]
                                                                                                                    Html.span [ prop.classes [ tw.``p-1`` ]
                                                                                                                                prop.children [ Bleeter.icon
                                                                                                                                                    "bi:dot"
                                                                                                                                                    "16" ] ] ] ]
                                                                                         Html.div [ prop.classes [ tw.flex
                                                                                                                   tw.``float-right``
                                                                                                                   tw.``w-8``
                                                                                                                   tw.``h-8``
                                                                                                                   tw.``rounded-full``
                                                                                                                   tw.``p-1``
                                                                                                                   tw.``pl-2``
                                                                                                                   tw.``pt-2``
                                                                                                                   tw.``cursor-pointer``
                                                                                                                   tw.``hover:bg-green-400`` ]
                                                                                                    prop.children [ Bleeter.icon
                                                                                                                        "ant-design:ellipsis-outlined"
                                                                                                                        "16" ] ] ] ]
                                                              Html.div [ prop.classes [ tw.flex ]
                                                                         prop.children [ Html.span [ prop.text
                                                                                                         bleet.Content ] ] ]
                                                              Html.div [ prop.classes [ tw.flex; tw.``py-4`` ]
                                                                         prop.children [ Html.div [ prop.classes [ tw.flex
                                                                                                                   tw.``flex-1`` ]
                                                                                                    prop.children [ Bleeter.icon
                                                                                                                        "ei:comment"
                                                                                                                        "24"
                                                                                                                    Html.text
                                                                                                                        bleet.Replies ] ]
                                                                                         Html.div [ prop.classes [ tw.flex
                                                                                                                   tw.``flex-1`` ]
                                                                                                    prop.children [ Bleeter.icon
                                                                                                                        "ei:retweet"
                                                                                                                        "24"
                                                                                                                    Html.text
                                                                                                                        bleet.Rebleets ] ]
                                                                                         Html.div [ prop.classes [ tw.flex
                                                                                                                   tw.``flex-1`` ]
                                                                                                    prop.children [ Bleeter.icon
                                                                                                                        "ei:heart"
                                                                                                                        "24"
                                                                                                                    Html.text
                                                                                                                        bleet.Likes ] ] ] ] ] ] ] ]

let bleetProfileElem (state: State) (dispatch:Msg->unit) =
    let profile = state.Profile
    let followBtn = 
        let noShowBtnClasses = [ tw.``hidden``]
        let yesFollowClasses = [ tw.``w-36`` ]
        let noFollowClasses = [ tw.``w-20`` ]
        let classes = [[
            tw.``rounded-full``
            tw.border
            tw.``h-10``
            tw.``mt-3``
            tw.``ml-2``
            tw.``border-green-500``
            tw.``text-green-500``
        ] ; (match profile.IsFollow with
            | None -> noShowBtnClasses
            | Some isFollow -> if isFollow then yesFollowClasses else noFollowClasses)] |> List.concat
        Html.button [ 
            prop.id "bleeter-follow"
            prop.classes classes
            prop.onClick (fun _ -> dispatch(Follow))
            prop.text (
                match profile.IsFollow with
                | None -> ""
                | Some isFollow -> if isFollow then "Following" else "Follow") ]
    
    let profileOptions = 
        let optionsElem = 
            Html.div [
                prop.children [
                    Html.div [
                        prop.onClick (fun _ -> dispatch(CloseProfileOptions))
                        prop.classes [
                            tw.``fixed`` 
                            tw.``inset-0`` 
                            tw.``h-full`` 
                            tw.``w-full``
                            (if state.ProfileOption.IsProfileOptionOpen then tw.``block`` else tw.``hidden``)
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.top (state.ProfileOption.Coordinates.Y |> int)
                            style.left (state.ProfileOption.Coordinates.X |> int)
                        ]
                        prop.classes [
                            tw.``w-28``
                            tw.``bg-white``
                            tw.``rounded-md``
                            tw.``overflow-hidden``
                            tw.``shadow-xl``
                            tw.``border``
                            tw.``border-solid``
                            (if state.ProfileOption.IsProfileOptionOpen then tw.``absolute`` else tw.``hidden``)
                        ]
                        prop.children [
                            Html.div [
                                prop.text "Meow"
                            ]
                        ]
                    ]
                ]
            ]
        Html.div [ 
            Html.div [
                prop.onClick (fun event -> 
                    // https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget
                    let coordinates = {X = event.currentTarget?offsetLeft; Y = event.currentTarget?offsetTop}
                    let coordinates = {X = coordinates.X - 80.0; Y = coordinates.Y + 30.0}
                    dispatch(OpenProfileOptions coordinates))
                prop.classes [  
                    tw.``rounded-full``
                    tw.border
                    tw.``h-10``
                    tw.``w-10``
                    tw.``mt-3``
                    tw.``p-3``
                    tw.``pl-3.5``
                    tw.``border-green-500``
                    tw.``text-green-500``
                    tw.``cursor-pointer``
                    tw.``hover:bg-green-400``
                    tw.``hover:text-gray-800`` ]
                prop.children [ Bleeter.icon "ant-design:ellipsis-outlined" "12"]
            ]
            optionsElem 
        ] 

    Html.div [ Html.img [ prop.classes [ tw.``w-full``
                                         tw.``bg-cover`` ]
                          prop.src profile.Banner ]
               Html.div [ prop.classes [ tw.flex
                                         tw.``flex-row`` ]
                          prop.children [ Html.img [ prop.classes [ tw.``h-24``
                                                                    tw.``w-24``
                                                                    tw.``rounded-full``
                                                                    tw.``border-4``
                                                                    tw.``border-gray-100``
                                                                    tw.``m-2``
                                                                    tw.flex ]
                                                     prop.src profile.ProfilePic ]
                                          Html.div [ prop.classes [ tw.``float-right``
                                                                    tw.flex
                                                                    tw.``flex-grow-2``
                                                                    tw.``flex-row-reverse``
                                                                    tw.``mr-2`` ]
                                                     prop.children [ followBtn; profileOptions ] ] ] ]
                                                                     

               Html.div [ prop.classes [ tw.flex; tw.``ml-2`` ]
                          prop.children [ Html.span [ prop.text profile.Name ] ] ]
               Html.div [ prop.classes [ tw.flex; tw.``ml-2`` ]
                          prop.children [ Html.span [ prop.text ("@" + profile.Handle) ] ] ]
               Html.div [ prop.classes [ tw.block; tw.``m-2`` ]
                          prop.children [ Html.div [ prop.classes [ tw.``inline-flex`` ]
                                                     prop.children [ Html.div [ prop.classes [ tw.``flex-1``
                                                                                               tw.``pt-1`` ]
                                                                                prop.children [ Bleeter.icon
                                                                                                    "akar-icons:location"
                                                                                                    "16" ] ]
                                                                     Html.div [ prop.classes [ tw.``flex-1`` ]
                                                                                prop.children [ Html.span [ prop.text
                                                                                                                profile.Location ] ] ] ] ]
                                          Html.div [ prop.classes [ tw.``inline-flex``
                                                                    tw.``ml-2`` ]
                                                     prop.children [ Html.div [ prop.classes [ tw.``flex-1``
                                                                                               tw.``pt-1`` ]
                                                                                prop.children [ Bleeter.icon
                                                                                                    "il:url"
                                                                                                    "12" ] ]
                                                                     Html.a [ prop.href profile.Url
                                                                              prop.target "_blank"
                                                                              prop.classes [ tw.``flex-1``
                                                                                             tw.underline
                                                                                             tw.``text-green-600`` ]
                                                                              prop.children [ Html.span [ prop.text (
                                                                                                              Bleeter.getUrl
                                                                                                                  profile.Url
                                                                                                          ) ] ] ] ] ] ] ]
               Html.div [ prop.classes [ tw.flex
                                         tw.``flex-row``
                                         tw.``m-2`` ]
                          prop.children [ Html.a [ prop.href "#"
                                                   prop.children [ Html.span [ prop.classes [ tw.``font-bold`` ]
                                                                               prop.text (string profile.Following) ]
                                                                   Html.span [ prop.classes [ tw.``ml-1`` ]
                                                                               prop.text "Following" ] ] ]
                                          Html.a [ prop.classes [ tw.``ml-2`` ]
                                                   prop.href "#"
                                                   prop.children [ Html.span [ prop.classes [ tw.``font-bold`` ]
                                                                               prop.text (string profile.Following) ]
                                                                   Html.span [ prop.classes [ tw.``ml-1`` ]
                                                                               prop.text "Followers" ] ] ] ] ] ]

let render (state: State) (dispatch:Msg->unit) =
    Html.div [ bleetProfileElem state  dispatch

               Html.div [ prop.classes [ tw.``text-2xl``
                                         tw.``h-12``
                                         tw.``border-b``
                                         tw.``border-gray-300``
                                         tw.``text-green-600`` ]
                          prop.children [ Html.span [ prop.classes [ tw.``m-6`` ]
                                                      prop.text "Latest Bleets" ] ] ]

               // let bleetList = [1..100] |> List.collect (fun x -> bleets |> (List.map bleetElem))
               let bleetList = state.Bleets |> (List.map bleetElem)

               Html.div [ prop.children bleetList ] ]
