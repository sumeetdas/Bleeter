[<RequireQualifiedAccess>]
module CreateBleet

open Elmish
open Feliz
open Tailwind
open Browser
open Browser.Types
// https://stackoverflow.com/questions/64194493/syntax-confusion-in-fable-with-eventtarget
open Fable.Core.JsInterop
open System
open Feliz.Router

type State =
    {
        Display: bool
        IsModal: bool
        Profile: Profile option
        Bleet: Bleet option
        BleetContent: string
        PreviousUrl: string list
    }

type Msg =
    | DisplayPage of Profile * string list
    | DisplayModal of Profile * string list
    | CloseModal
    | UpdateBleetContent of string
    | OutsideModalClickClose of string
    | AddBleet

let init () =
    {
        Display = false
        IsModal = false
        Profile = None
        Bleet = None
        BleetContent = ""
        PreviousUrl = []
    }

let closeModal (state: State) =
    let initState = init ()
    Router.navigate (state.PreviousUrl |> List.toArray)
    { initState with PreviousUrl = state.PreviousUrl }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    match msg with
    | DisplayPage (profile, previousUrl) ->
        { state with
            Display = true
            IsModal = false
            Profile = Some profile
            PreviousUrl = previousUrl
        },
        Cmd.none
    | DisplayModal (profile, previousUrl) ->
        { state with
            Display = true
            IsModal = true
            Profile = Some profile
            PreviousUrl = previousUrl
        },
        Cmd.none
    | CloseModal -> closeModal state, Cmd.none
    | UpdateBleetContent content -> { state with BleetContent = content }, Cmd.none
    | OutsideModalClickClose id ->
        if (state.Display
            && id = "CreateBleetModal"
            && state.BleetContent.Length = 0) then
            let state = closeModal state
            state, Cmd.none
        else
            state, Cmd.none
    | AddBleet ->
        let bleet =
            match state.Profile with
            | None -> None
            | Some profile ->
                let randomId = Random().Next(1000, 10_000_000)

                {
                    Id = randomId
                    Name = profile.Name
                    Content = state.BleetContent
                    ProfilePic = profile.ProfilePic
                    Handle = profile.Handle
                    Time = ""
                    Rebleets = 0
                    Likes = 0
                    Replies = 0
                    RepliesType = Some Boring
                    IsMyBleet = true
                }
                |> Some

        { state with Bleet = bleet }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let profile = state.Profile

    let modalClasses =
        [
            tw.``fixed``
            tw.``h-full``
            tw.``w-full``
            tw.``inset-0``
            tw.``z-50``
            tw.``overflow-hidden``
            tw.flex
            tw.``justify-center``
            tw.``items-center``
            tw.``bg-black``
            tw.``bg-opacity-75``
        ]

    let displayModalClass = if state.Display then [ tw.block ] else [ tw.hidden ]

    let classes = [ modalClasses; displayModalClass ] |> List.concat

    let createBleetElem =
        Html.div [
            prop.id "CreateBleetModal"
            prop.onClick (fun event -> dispatch (OutsideModalClickClose event.target?id))
            prop.classes classes
            prop.children [
                Html.div [
                    prop.id "CreateBleetModalMain"
                    prop.classes [
                        tw.border
                        tw.``border-blue-500``
                        tw.``shadow-lg``
                        tw.``bg-white``
                        tw.``md:max-w-md``
                        tw.``mx-auto``
                        tw.rounded
                        tw.``shadow-lg``
                        tw.``overflow-y-auto``
                        tw.``text-left``
                        tw.``py-4``
                        tw.``px-6``
                        tw.``w-2/4``
                        tw.``h-48``
                        tw.flex
                        tw.``flex-col``
                    ]
                    prop.children [
                        Html.button [
                            prop.classes [
                                tw.flex
                                tw.``flex-row``
                            ]
                            prop.children [
                                Bleeter.icon "akar-icons:cross" "12"
                            ]
                            prop.onClick (fun _ -> dispatch (CloseModal))
                        ]
                        Html.div [
                            prop.classes [
                                tw.flex
                                tw.``flex-row``
                                tw.``mt-4``
                            ]
                            prop.children [
                                Html.img [
                                    prop.classes [
                                        tw.flex
                                        tw.``flex-grow-0``
                                        tw.``mr-2``
                                        tw.``h-8``
                                        tw.``w-8``
                                        tw.``rounded-full``
                                        tw.``border-2``
                                        tw.``border-gray-100``
                                    ]
                                    prop.src (profile |> Option.fold (fun _ p -> p.ProfilePic) "")
                                ]
                                Html.div [
                                    prop.classes [
                                        tw.flex
                                        tw.``flex-grow-1``
                                        tw.``flex-col``
                                    ]
                                    prop.children [
                                        Html.textarea [
                                            prop.id "create-bleet-text"
                                            prop.classes [
                                                tw.``resize-none``
                                                tw.``outline-none``
                                            ]
                                            prop.rows 3
                                            prop.value state.BleetContent
                                            // https://stackoverflow.com/questions/64194493/syntax-confusion-in-fable-with-eventtarget
                                            // https://stackoverflow.com/a/52338979
                                            prop.onChange
                                                (fun (ev: Event) ->
                                                    dispatch (UpdateBleetContent(ev.target?value |> string)))
                                            prop.placeholder "Mehehe?"
                                        ]
                                        Html.div [
                                            prop.classes [
                                                tw.flex
                                                tw.``flex-row-reverse``
                                            ]
                                            prop.children [
                                                Html.button [
                                                    prop.id "create-bleet-button"
                                                    prop.classes [
                                                        tw.``bg-green-500``
                                                        tw.``text-white``
                                                        tw.``rounded-md``
                                                        tw.``px-8``
                                                        tw.``py-2``
                                                        tw.``w-24``
                                                        tw.``float-right``
                                                        tw.``text-base``
                                                        tw.``font-medium``
                                                        tw.``hover:bg-green-600``
                                                        tw.``focus:outline-none``
                                                        tw.``focus:ring-2``
                                                        tw.``focus:ring-green-300``
                                                    ]
                                                    prop.onClick (fun _ -> dispatch AddBleet)
                                                    prop.text "Bleet"
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

                ]
        ]

    Html.div [
        prop.classes [ tw.``ml-3`` ]
        prop.children [ createBleetElem ]
    ]
