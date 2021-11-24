[<RequireQualifiedAccess>]
module Home

open Elmish
open Feliz
open Tailwind

type State = { Count: int; BleetElems: BleetElem.State list }

type Msg = BleetElemMsg of int * BleetElem.Msg


let init () =
    let bleets: Bleet list =
        [
            {
                Id = 1
                Name = "Bleeter Boi"
                Content = "Hello Bleeter!"
                ProfilePic = "/bleeter_profile_pic.png"
                Handle = "BleeterBoi"
                Time = ""
                Rebleets = 123
                Likes = 3000
                Replies = 0
            }
            {
                Id = 2
                Name = "Sheeple"
                Content = "We the Sheeple!"
                ProfilePic = "/bleeter_profile_pic.png"
                Handle = "Sheeple"
                Time = ""
                Rebleets = 1230
                Likes = 40000
                Replies = 1
            }
            {
                Id = 3
                Name = "John Xina"
                Content = "“The enemy can’t hit what they can’t see.”- John Xina, the art of war"
                ProfilePic = "/john_xina.png"
                Handle = "JohnXina"
                Time = ""
                Rebleets = 1230
                Likes = 40000
                Replies = 1
            }
        ]

    {
        Count = 0
        BleetElems = (bleets |> List.map BleetElem.init)
    }


let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | BleetElemMsg (id, msg) ->
        match msg with
        | BleetElem.Msg.ReportBleet ->
            printf "Report"
            state, Cmd.none
        | _ ->
            state.BleetElems
            |> List.tryFind (fun bleetElem -> bleetElem.Bleet.Id = id)
            |> (fun bleetElemOpt ->
                match bleetElemOpt with
                | None -> state, Cmd.none
                | Some bleetElem ->
                    let newBleetElem, cmd = BleetElem.update msg bleetElem

                    let newBleetElems =
                        state.BleetElems
                        |> (List.updateAt (fun elem -> elem.Bleet.Id = id) newBleetElem)

                    { state with BleetElems = newBleetElems }, (Cmd.map (fun msg -> BleetElemMsg(id, msg)) cmd))

let render (state: State) (dispatch: Msg -> unit) =
    let bleetElemList =
        state.BleetElems
        |> List.map
            (fun bleetElem ->
                BleetElem.render
                    bleetElem
                    ((fun msg -> BleetElemMsg(bleetElem.Bleet.Id, msg))
                     >> dispatch))

    Html.div [
        prop.classes [
            tw.flex
            tw.``flex-col``
            tw.``flex-grow-1``
        ]
        prop.children [
            Html.div [
                prop.classes [ tw.flex; tw.``h-20`` ]
                prop.text "Home"
            ]
            Html.div [
                prop.classes [
                    tw.flex
                    tw.``flex-col``
                ]
                prop.children bleetElemList
            ]
        ]
    ]
