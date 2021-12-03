[<RequireQualifiedAccess>]
module BleetElem

open Elmish
open Feliz
open Tailwind

type Msg =
    | BleetOptionMsg of Msg EllipsisOption.Msg
    | DeleteBleet
    | ReportBleet

type State =
    {
        Bleet: Bleet
        BleetOption: Msg EllipsisOption.State
        IsDeleted: bool
    }

let init bleet =
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
            tw.``cursor-pointer``
            tw.``hover:bg-green-400``
            tw.flex
            tw.``float-right``
        ]

    let offset: Coordinates = { X = -100.0; Y = 0.0 }

    {
        Bleet = bleet
        BleetOption = EllipsisOption.init options 16 cssClasses offset
        IsDeleted = false
    }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DeleteBleet ->
        if state.Bleet.IsMyBleet then
            { state with IsDeleted = true }, Cmd.none
        else
            state, Cmd.none
    | ReportBleet ->
        printf "report bleet"
        let bleetOption, bleetOptionCmd = EllipsisOption.update (EllipsisOption.Close) state.BleetOption
        { state with BleetOption = bleetOption }, bleetOptionCmd
    | BleetOptionMsg msg ->
        let bleetOption, bleetOptionCmd = EllipsisOption.update msg state.BleetOption
        { state with BleetOption = bleetOption }, bleetOptionCmd

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
                        prop.classes [ tw.flex ]
                        prop.children [
                            Html.span [ prop.text bleet.Content ]
                        ]
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
