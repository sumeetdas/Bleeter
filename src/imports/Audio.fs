[<RequireQualifiedAccess>]
module Audio

open Fable.Core
open Fable.Core.JsInterop

/// Uses Fable's Emit to call JavaScript directly and play sounds
[<Emit("(new Audio($0)).play();")>]
let private play (fileName: string) = jsNative

let playBleat () = play "bleat.mp3"
