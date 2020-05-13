namespace MF.Playground

[<RequireQualifiedAccess>]
module Ev3Testing =
    let rec connect () = async {
        try
            return!
                Ev3Brick.IPAddress "192.168.1.121"
                |> Ev3Brick.Wifi
                |> Ev3Brick.connect
        with
        | _ ->
            printfn "Waiting for brick ..."
            do! Async.Sleep 1000
            return! connect()
    }
