namespace MF.Playground

[<RequireQualifiedAccess>]
module Ev3Brick =
    open Lego.Ev3.Core
    open Lego.Ev3.Desktop

    [<RequireQualifiedAccess>]
    type SingleOutput =
        | A
        | B
        | C
        | D

    [<RequireQualifiedAccess>]
    module private SingleOutput =
        let rec outputPort = function
            | SingleOutput.A -> OutputPort.A
            | SingleOutput.B -> OutputPort.B
            | SingleOutput.C -> OutputPort.C
            | SingleOutput.D -> OutputPort.D

    [<RequireQualifiedAccess>]
    type Output =
        | A
        | B
        | C
        | D
        | All
        | Custom of SingleOutput list

    [<RequireQualifiedAccess>]
    module private Output =
        let rec outputPort = function
            | Output.A -> [ OutputPort.A ]
            | Output.B -> [ OutputPort.B ]
            | Output.C -> [ OutputPort.C ]
            | Output.D -> [ OutputPort.D ]
            | Output.All -> [ OutputPort.All ]
            | Output.Custom [] -> []
            | Output.Custom outputs ->
                match outputs |> List.distinct |> List.sort with
                | [ SingleOutput.A; SingleOutput.B; SingleOutput.C; SingleOutput.D ] -> [ OutputPort.All ]
                | ports -> ports |> List.map SingleOutput.outputPort

    [<RequireQualifiedAccess>]
    type Direction =
        | Up
        | Down

    [<RequireQualifiedAccess>]
    module private Direction =
        let apply value = function
            | Direction.Up -> value
            | Direction.Down -> value * -1

    type IPAddress = IPAddress of string
    type BluetoothPort = BluetoothPort of string

    type Connection =
        | Wifi of IPAddress
        | Bluetooth of BluetoothPort
        | USB

    //let sayHello (brick: Brick) =
        //brick.DirectCommand.

    let makeBeep (brick: Brick) =
        brick.DirectCommand.PlayToneAsync(100, uint16 1000, uint16 300)
        |> Async.AwaitTask

    let startMotorsAtOpositeSpeed (outputA, outputB) speed time (brick: Brick) =
        async {
            printfn "Start motors at %d %%" speed

            printfn " -> turn %A" outputA
            brick.BatchCommand.TurnMotorAtSpeedForTime(outputA |> SingleOutput.outputPort, speed, uint32 time, false)
            printfn " -> turn %A" outputB
            brick.BatchCommand.TurnMotorAtSpeedForTime(outputB |> SingleOutput.outputPort, -speed, uint32 time, false)

            printfn " -> send"
            let! _ = brick.BatchCommand.SendCommandAsync() |> Async.AwaitTask
            printfn " -> done"
            return()
        }

    let startMotors output speed time (brick: Brick) =
        async {
            match output |> Output.outputPort with
            | [] -> return ()
            | [ output ] ->
                printf "Turn motor %A at speed %A ... " output speed
                do! brick.DirectCommand.TurnMotorAtSpeedForTimeAsync(output, speed, uint32 time, false) |> Async.AwaitTask
                printfn "Turned!"
            | outputs ->
                printf "Turn motors %A at speed %A ... " output speed
                outputs
                |> List.iter(fun output ->
                    brick.BatchCommand.TurnMotorAtSpeedForTime(output, speed, uint32 time, true)
                )
                let! _ = brick.BatchCommand.SendCommandAsync() |> Async.AwaitTask
                printfn "Turned!"
                return()
        }

    let stopMotors (brick: Brick) =
        async {
            printfn "Stop motors!"
            do! brick.DirectCommand.StopMotorAsync(OutputPort.All, false) |> Async.AwaitTask
            return()
        }

    let breakMotors (brick: Brick) =
        async {
            printfn "Break motors!"
            do! brick.DirectCommand.StopMotorAsync(OutputPort.All, true) |> Async.AwaitTask
            return()
        }

    let turnMotor90 direction output (brick: Brick) = async {
        let steps = 90 |> uint32
        let power = direction |> Direction.apply 100

        match output |> Output.outputPort with
        | [] -> return ()
        | [ output ] ->
            printf "Turn motor %A %A by 90 degres ... " output direction
            do! brick.DirectCommand.StepMotorAtPowerAsync(output, power, steps, true) |> Async.AwaitTask
            printfn "Turned!"
        | outputs ->
            printf "Turn motors %A %A by 90 degres ... " output direction
            outputs
            |> List.iter(fun output ->
                brick.BatchCommand.StepMotorAtPower(output, power, steps, true)
            )
            let! _ = brick.BatchCommand.SendCommandAsync() |> Async.AwaitTask
            printfn "Turned!"
            return()
    }

    let connect connection = async {
        let brick =
            match connection with
            | Wifi (IPAddress ipAddress) -> Brick(NetworkCommunication(ipAddress))
            | Bluetooth (BluetoothPort port) -> Brick(BluetoothCommunication(port))
            | USB -> Brick(UsbCommunication())

        printfn "[Brick] Connecting ..."
        do! brick.ConnectAsync() |> Async.AwaitTask
        printfn "[Brick] connected"

        return brick
    }

    let configure connection =
        async {
            printfn "Connecting ..."

            try
                let! brick = connect connection

                do! brick |> makeBeep

                do! brick |> startMotorsAtOpositeSpeed (SingleOutput.A, SingleOutput.B) 30 2000
                do! Async.Sleep 2000
                do! brick |> startMotorsAtOpositeSpeed (SingleOutput.A, SingleOutput.B) 60 2000
                do! Async.Sleep 2000
                do! brick |> startMotorsAtOpositeSpeed (SingleOutput.A, SingleOutput.B) 90 5000
                do! Async.Sleep 2000
                do! brick |> stopMotors

                printfn "Tone played!"
            with
            | e -> eprintfn "Error %A" e
        }
