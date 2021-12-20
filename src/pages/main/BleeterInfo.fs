[<RequireQualifiedAccess>]
module BleeterInfo

open Feliz
open Tailwind

let page =
    let mainHeading = MainLayout.heading "Everything is about you"

    let toPara (lines: string list) : string = lines |> String.concat " "

    let toParaElem (text: string) =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-col``
                tw.``w-full``
                tw.``mb-6``
            ]
            prop.text text
        ]

    let toContent (paraList: string list) =
        Html.div [
            prop.classes [
                tw.flex
                tw.``flex-col``
                tw.``w-auto``
                tw.``m-4``
            ]
            prop.children (paraList |> List.map toParaElem)

            ]

    let toLinkString (text: string, href: string) = sprintf "<a text='%s' href='%s' target='_blank'></a>" text href

    let bleeterInfoContent =
        let para1 =
            [
                "Information isn't about imparting knowledge anymore."
                "The internet changed all that."
            ]
            |> toPara

        let para2 =
            [
                "Welcome to world of self-aggrandizing shorthand."
                "Keep strangers and people you hated in high school"
                "up to date with every mundane details of your life 24/7."
                "Welcome to the delusion of having an interesting"
                "life and friends."
            ]
            |> toPara

        let para3 =
            [
                "Bleeter is the perfect storm of blogging, social"
                "networking and text messaging. We're demolishing"
                "100,000 years of complex linguistic development"
                "280 characters at a time."
            ]
            |> toPara

        [ para1; para2; para3 ] |> toContent

    let coreComponents = [ mainHeading; bleeterInfoContent ]

    MainLayout.elem (Some "/img/bleeter-logo.png") coreComponents
