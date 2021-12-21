```f#
// to print to console, use `printfn` function
printfn "Hello console!"

// to force use of module name to invoke module function, use [<RequireQualifiedAccess>]
[<RequireQualifiedAccess>]
module Counter

// this way, to invoke render function of Counter, you need to use `Counter.render()` instead of simple `render()` 


(* compile order in fsproj matters. If 
    <Compile Include="src\components\*.fs" />
   is put AFTER 
   <Compile Include="src\App.fs" />
   then we won't be able to use modules defined in `components` directory
*) 

// elmish arch
module App

open Elmish
open Elmish.React
open Feliz

// data model
type State =
    { Count: int }

// events
type Msg = 
    | Increment
    | Decrement

// need parentheses for indicating that init is a function
let init() = { Count = 0 }

let update (msg: Msg) (state: State): State =
    match msg with 
    | Increment -> {state with Count = state.Count + 1}
    | Decrement -> {state with Count = state.Count - 1}

let render (state: State) (dispatch: Msg -> Unit) =
    Html.div [
        Html.button [
            prop.onClick (fun _ -> dispatch Increment)
            prop.text "Increment"
        ]

        Html.button [
            prop.onClick (fun _ -> dispatch Decrement)
            prop.text "Decrement"
        ]

        Html.h1 state.Count
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "bleeter-app"
|> Program.run
```


* To upgrade fable:
    * update package number in `App.fsproj` for `fable-core` and in `.config/dotnet-tools.json` to latest (here 3.6.3)

    * update dotnet tool via 
    ```
    dotnet new tool-manifest # if you are setting up this repo
    dotnet tool install --local Fable --version 3.6.3
    dotnet tool install --global Fable --version 3.6.3
    ```
    * If s# goes south, then clear nuget packages via deleting everything in `~/.nuget/packages`
