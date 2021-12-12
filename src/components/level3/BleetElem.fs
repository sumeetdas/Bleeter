[<RequireQualifiedAccess>]
module BleetElem

open Elmish
open Feliz
open Tailwind
open System.Text.RegularExpressions

type Msg =
    | BleetOptionMsg of Msg EllipsisOption.Msg
    | DeleteBleet
    | ReportBleet
    | UrlChanged of string list

type State =
    {
        Bleet: Bleet
        BleetOption: Msg EllipsisOption.State
        IsDeleted: bool
        NotifMsg: ReactElement option
        ModalMsg: Modal.Msg
        ReportCount: int option
        PreviousUrl: string list
    }

let init (previousUrl: string list) bleet =
    let options: Msg EllipsisOption.Option list =
        [
            { Name = "Delete"; Command = Cmd.ofMsg DeleteBleet }
            { Name = "Report"; Command = Cmd.ofMsg ReportBleet }
        ]

    let options =
        options
        |> List.removeBy (fun opt -> opt.Name = "Delete" && (not bleet.IsMyBleet))

    let cssClasses =
        [
            tw.``rounded-full``
            tw.``bleeter-pointer``
            tw.``hover:bg-green-500``
            tw.flex
            tw.``float-right``
            tw.``hover:text-white``
            tw.``text-xl``
        ]

    let offset: Coordinates = { X = -100.0; Y = 0.0 }

    {
        Bleet = bleet
        BleetOption = EllipsisOption.init options 16 cssClasses offset
        IsDeleted = false
        NotifMsg = None
        ModalMsg = Modal.DoNothing
        ReportCount = None
        PreviousUrl = previousUrl
    }

let report (state: State) (first: string, second: string, modalMsg: Modal.Msg) : State * Msg Cmd =
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

    let bleetOption, bleetOptionCmd = EllipsisOption.update (EllipsisOption.Close) nextState.BleetOption
    { nextState with BleetOption = bleetOption }, bleetOptionCmd

let update (msg: Msg) (state: State) : State * Msg Cmd =
    let state = { state with NotifMsg = None; ModalMsg = Modal.DoNothing }
    match msg with
    | UrlChanged url -> 
        { state with PreviousUrl = url }, Cmd.none
    | DeleteBleet ->
        if state.Bleet.IsMyBleet then
            { state with IsDeleted = true }, Cmd.none
        else
            state, Cmd.none
    | ReportBleet ->
        let content = state.Bleet.Content
        let notPermitted = content.Contains("Xiowei") || content.Contains("Xina")
            
        if notPermitted then
            let first = "Operation not permitted!"
            let second = "OPERATION NOT PERMITTED!!!"

            let modalMsg =
                Modal.ShowCCP state.PreviousUrl

            report state (first, second, modalMsg)
        else
            let first = sprintf "We've reported @%s's bleet to Bleeter police." state.Bleet.Handle
            let second = "We get it. You are pissed."

            let modalMsg =
                Modal.ShowMeditation state.PreviousUrl

            report state (first, second, modalMsg)
    | BleetOptionMsg msg ->
        let bleetOption, bleetOptionCmd = EllipsisOption.update msg state.BleetOption
        { state with BleetOption = bleetOption }, bleetOptionCmd

let ytEmbed (src: string) =
    Html.div [
        prop.classes [ tw.``iframe-container`` ]
        prop.children [
            Html.iframe [
                prop.custom ("data-src", src)
                prop.src src
                prop.custom ("frameBorder", "0")
                prop.custom ("allow", "accelerometer; autoplay")
                prop.custom ("allowFullScreen", true)
            ]
        ]
    ]

let getLink (url: string) (name: string) =
    Html.a [
        prop.classes [
            tw.``text-green-500``
            tw.``ml-1``
        ]
        prop.target "_blank"
        prop.href url
        prop.text name
    ]

let getTagLink (tag: string) =
    let tagName = tag.Replace("#", "")

    Html.a [
        prop.classes [
            tw.``text-green-500``
            tw.``ml-1``
        ]
        prop.href ("#/tags/" + tagName)
        prop.text tag
    ]

let getBleetContent (content: string) : ReactElement =
    let words = content.Split [| ' ' |]

    words
    |> Seq.indexed
    |> Seq.fold
        (fun (content: ReactElement list, accText: string, ytThumbUrl: string option) (pair: int * string) ->
            let index, word = pair

            if word.Contains "youtube" then
                let ytThumbUrl = Some word
                let content = content @ [ Html.span [ prop.text accText ] ]
                let accText = ""
                let urlName = Regex.Replace(word, "http(s?)://", "")

                let content =
                    if index < words.Length - 1 then
                        content @ [ getLink word urlName ]
                    else
                        content

                (content, accText, ytThumbUrl)
            else if word.StartsWith "http" || word.StartsWith "www" then
                let content = content @ [ Html.span [ prop.text accText ] ]
                let urlName = Regex.Replace(word, "http(s?)://", "")
                let content = content @ [ getLink word urlName ]
                let accText = ""
                (content, accText, ytThumbUrl)
            else if word.StartsWith "#" then
                let content = content @ [ Html.span [ prop.text accText ] ]
                let content = content @ [ getTagLink word ]
                let accText = ""
                (content, accText, ytThumbUrl)
            else
                // decided to concat multiple whitespaces into one ¯\_(ツ)_/¯
                let accText = accText + " " + word

                let content =
                    if index = words.Length - 1 then
                        content @ [ Html.span [ prop.text accText ] ]
                    else
                        content

                (content, accText, ytThumbUrl))
        ([], "", None)
    |> (fun (content: ReactElement list, _: string, ytThumbUrl: string option) ->
        match ytThumbUrl with
        | Some url ->
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                    tw.``w-full``
                ]
                prop.children [
                    Html.p [
                        prop.classes [ tw.``flex-shrink`` ]
                        prop.children content
                    ]
                    // Html.div [
                    //     prop.classes [ tw.flex; tw.``flex-row`` ]
                    //     prop.children (ytEmbed url)
                    // ]
                    ]
            ]
        | None ->
            Html.p [
                prop.classes [ tw.``flex-shrink`` ]
                prop.children content
            ])

let render (state: State) (dispatch: Msg -> unit) =
    let bleet = state.Bleet

    Html.article [
        prop.key state.Bleet.Id
        prop.id ("bleet-" + (state.Bleet.Id |> string))
        prop.classes [
            tw.``hover:bg-green-100``
            tw.flex
            tw.``flex-row``
            tw.``p-4``
            tw.``pb-0``
            tw.``border-b``
            tw.``border-gray-300``
            tw.``bg-gray-100``
        ]
        prop.children [
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-grow-0``
                    tw.``w-12``
                ]
                prop.children [
                    Html.img [
                        prop.classes [
                            tw.``h-12``
                            tw.``w-12``
                            tw.``rounded-full``
                            tw.``border-2``
                            tw.``border-green-100``
                        ]
                        prop.src bleet.ProfilePic
                    ]
                ]
            ]
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                    tw.``flex-1``
                    tw.``pl-2``
                ]
                prop.children [
                    Html.div [
                        prop.classes [
                            tw.flex
                            tw.``flex-row``
                        ]
                        prop.children [
                            Html.div [
                                prop.classes [
                                    tw.flex
                                    tw.``flex-grow-1``
                                ]
                                prop.children [
                                    Html.a [
                                        prop.href (sprintf "#/%s" bleet.Handle)
                                        prop.children [
                                            Html.span [
                                                prop.classes [ tw.``hover:underline`` ]
                                                prop.text bleet.Name
                                            ]
                                            Html.span [
                                                prop.classes [ tw.``ml-1`` ]
                                                prop.text ("@" + bleet.Handle)
                                            ]
                                        ]
                                    ]

                                    // date will be implemented later
                                    // Html.span [
                                    //     prop.classes [ tw.``p-1`` ]
                                    //     prop.children [
                                    //         Bleeter.icon "bi:dot" "16"
                                    //     ]
                                    // ]
                                    ]
                            ]
                            EllipsisOption.render state.BleetOption (BleetOptionMsg >> dispatch)
                        ]
                    ]
                    Html.div [
                        prop.classes [
                            tw.flex
                            tw.``flex-row``
                            tw.``h-full``
                            tw.``w-full``
                        ]
                        prop.children (getBleetContent bleet.Content)
                    ]
                    Html.div [
                        prop.classes [ tw.flex; tw.``py-4`` ]
                        prop.children [
                            Html.a [
                                prop.classes [ tw.flex; tw.``flex-1`` ]
                                prop.href (sprintf "#/%s/bleets/%d" bleet.Handle bleet.Id)
                                prop.children [
                                    Bleeter.icon "ei:comment" "24"
                                    Html.text bleet.Replies
                                ]
                            ]
                            Html.div [
                                prop.classes [ tw.flex; tw.``flex-1`` ]
                                prop.children [
                                    Bleeter.icon "ei:retweet" "24"
                                    Html.text bleet.Rebleets
                                ]
                            ]
                            Html.div [
                                prop.classes [ tw.flex; tw.``flex-1`` ]
                                prop.children [
                                    Bleeter.icon "ei:heart" "24"
                                    Html.text bleet.Likes
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
