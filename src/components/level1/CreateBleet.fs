[<RequireQualifiedAccess>]
module CreateBleet

open Elmish
open Feliz
open Tailwind
open Browser.Types
// https://stackoverflow.com/questions/64194493/syntax-confusion-in-fable-with-eventtarget
open Fable.Core.JsInterop
open System

type State =
    {
        Display: bool
        Profile: Profile
        Bleet: Bleet option
        BleetContent: string
    }

type Msg =
    | Display
    | UpdateBleetContent of string
    | OutsideModalClickClose
    | AddBleet
    | DataUpdated of Profile

let init () =
    {
        Display = false
        Profile = Profile.init ()
        Bleet = None
        BleetContent = ""
    }

let update (msg: Msg) (state: State) : State * Msg Cmd =
    // clear up transient state
    let state = { state with Bleet = None }

    match msg with
    | DataUpdated profile -> { state with Profile = profile }, Cmd.none
    | OutsideModalClickClose -> { state with Display = false }, Cmd.none
    | Display -> { state with Display = true }, Cmd.none
    | UpdateBleetContent content -> { state with BleetContent = content }, Cmd.none
    | AddBleet ->
        let bleet =
            let randomId = Random().Next(1000, 10_000_000)
            let profile = state.Profile

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
                RepliesType = Some RepliesType.Boring
                IsMyBleet = true
            }
            |> Some

        { state with Bleet = bleet }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let profile = state.Profile

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-row``
            tw.``mt-12``
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
                prop.src profile.ProfilePic
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
                        prop.onChange (fun (ev: Event) -> dispatch (UpdateBleetContent(ev.target?value |> string)))
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
