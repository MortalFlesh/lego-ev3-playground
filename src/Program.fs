open System

open MF.XBoxController
open MF.Playground

[<EntryPoint>]
let main argv =
    printfn "Start ..."

    //configureBrick |> Async.RunSynchronously

    // KeyBoardState.initial |> KeyBoardState.tryKeyboard |> Async.RunSynchronously
    // Transmission.test |> Async.RunSynchronously
    //Steering.test |> Async.RunSynchronously
    Display.test |> Async.RunSynchronously

    printfn "Done!"
    0
