[<RequireQualifiedAccess>]
module EllipsisOption

open Elmish
open Feliz
open Tailwind
// for `?` operator
open Fable.Core.JsInterop

type 'a Msg =
    | Close
    | Open of Coordinates
    | CommandMsg of 'a Cmd

type 'a Option = { Name: string; Command: 'a Cmd }

type 'a State =
    { IsOptionOpen: bool
      Coordinates: Coordinates
      Options: 'a Option list }

let init options =
    { IsOptionOpen = false
      Coordinates = { X = 0 |> float; Y = 0 |> float }
      Options = options }

let update (msg: 'a Msg) (state: 'a State) : 'a State * 'a Cmd =
    match msg with
    | CommandMsg cmd -> state, cmd
    | Close -> { state with IsOptionOpen = false }, Cmd.none
    | Open coordinates ->
        printf "%A" coordinates

        { state with
              IsOptionOpen = true
              Coordinates = coordinates },
        Cmd.none

let render (state: 'a State) (dispatch: 'a Msg -> unit) =
    let optionList =
        state.Options
        |> List.map
            (fun opt ->
                Html.div [ prop.text opt.Name
                           prop.onClick (fun _ -> dispatch (CommandMsg opt.Command)) ])

    let optionsElem =
        Html.div [ prop.children [ Html.div [ prop.onClick (fun _ -> dispatch (Close))
                                              prop.classes [ tw.``fixed``
                                                             tw.``inset-0``
                                                             tw.``h-full``
                                                             tw.``w-full``
                                                             (if state.IsOptionOpen then tw.block else tw.hidden) ] ]
                                   Html.div [ prop.style [ style.top (state.Coordinates.Y |> int)
                                                           style.left (state.Coordinates.X |> int) ]
                                              prop.classes [ tw.``w-28``
                                                             tw.``bg-white``
                                                             tw.``rounded-md``
                                                             tw.``overflow-hidden``
                                                             tw.``shadow-xl``
                                                             tw.border
                                                             tw.``border-solid``
                                                             (if state.IsOptionOpen then
                                                                  tw.absolute
                                                              else
                                                                  tw.hidden) ]
                                              prop.children optionList ] ] ]

    Html.div [ Html.div [ prop.onClick
                              (fun event ->
                                  // https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget
                                  let coordinates =
                                      { X = event.currentTarget?offsetLeft
                                        Y = event.currentTarget?offsetTop }

                                  let coordinates =
                                      { X = coordinates.X - 80.0; Y = coordinates.Y + 30.0 }

                                  dispatch (Open coordinates))
                          prop.classes [ tw.``rounded-full``
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
                          prop.children [ Bleeter.icon "ant-design:ellipsis-outlined" "12" ] ]
               optionsElem ]
