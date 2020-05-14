namespace MF.Playground

[<RequireQualifiedAccess>]
module Controller =
    open MF.XBoxController

    let configure onButtonPressed onPositionChanged =
        async {
            let! controller = Controller.waitFor Controller.Any

            printfn "Controller %A is connected." controller

            let! _ =
                [
                    controller |> Controller.onButtonPressedAsync Low onButtonPressed

                    controller |> Controller.onPositionChangedAsync Medium onPositionChanged
                ]
                |> Async.Parallel

            return ()
        }
