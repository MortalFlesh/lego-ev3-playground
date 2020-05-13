open System

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

open MF.XBoxController

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

let configureBrick =
    async {
        let! brick = Ev3Testing.connect()
        let motorsTimeout = 10000

        do!
            Controller.configure
                (function
                    | A -> brick |> Ev3Brick.stopMotors |> Async.Start
                    | B -> brick |> Ev3Brick.makeBeep |> Async.Start
                    | Rb -> brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Up Ev3Brick.Output.C |> Async.Start    // todo - add "memory" to allow only
                    | Lb -> brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Down Ev3Brick.Output.C |> Async.Start  // [1 -> 4] (not cycle directly 4<->1)
                    | _button ->
                        //printfn "Button pressed %A" button
                        ()
                )
                (function
                    | PositionChanged.Lt (TriggerPressedPower power) ->
                        printfn "Lt -> %A" power
                        brick
                        |> Ev3Brick.startMotorsAtOpositeSpeed
                            (Ev3Brick.SingleOutput.A, Ev3Brick.SingleOutput.B)
                            (int power)
                            motorsTimeout
                        |> Async.Start

                    | PositionChanged.ThumbPadLeft { X = x; Y = y } ->
                        printfn "ThumbPadLeft X: %A, Y: %A" x y

                        let power =
                            let y = int y
                            if y > 50 then (y - 50) * 2
                                (*
                                    50 -> 0
                                    52 -> 4
                                    60 -> 20
                                    100 -> 100
                                 *)
                            elif y < 50 then (50 - y) * -2
                                (*
                                    50 -> 0
                                    48 -> -4
                                    40 -> -20
                                    0 -> -100
                                 *)
                            else 0

                        let steeringPower =
                            let x = int x
                            if x > 50 then x - 50
                            elif x < 50 then 50 - x
                            else 0

                        brick
                        |> Ev3Brick.startMotorsAtOpositeSpeed
                            (Ev3Brick.SingleOutput.A, Ev3Brick.SingleOutput.B)
                            (int power)
                            motorsTimeout
                        |> Async.Start

                        brick
                        |> Ev3Brick.startMotors
                            Ev3Brick.Output.D
                            (int steeringPower)
                            motorsTimeout
                        |> Async.Start

                    | PositionChanged.Rt (TriggerPressedPower power) ->
                        printfn "Rt -> %A" power
                        brick
                        |> Ev3Brick.startMotorsAtOpositeSpeed
                            (Ev3Brick.SingleOutput.A, Ev3Brick.SingleOutput.B)
                            (int power * -1)
                            motorsTimeout
                        |> Async.Start

                    //| PositionChanged.ThumbPadLeft { X = x; Y = y } -> printfn "ThumbPadLeft X: %A, Y: %A" x y
                    //| PositionChanged.ThumbPadRight { X = x; Y = y } -> printfn "ThumbPadRight X: %A, Y: %A" x y
                    | _ -> ()
                )
    }

let percent = function
    | min when min <= 0 -> 0
    | max when max >= 100 -> 100
    | value -> value

type Direction =
    | Forward | Backward
    | Left | Right
    | ForwardLeft | ForwardRight
    | BackwardLeft | BackwardRight

type KeyBoardState = {
    CurrentSpeed: int
    Direction: Direction option
}

[<RequireQualifiedAccess>]
module KeyBoardState =
    (* [<RequireQualifiedAccess>]
    module Action =
        let forward *)

    let update action (state: KeyBoardState): KeyBoardState =
        state |> action

    let speedStep = 10

    let initial = {
        CurrentSpeed = 0
        Direction = None
    }

    let rec tryKeyboard state = async {
        let key = Console.ReadKey().Key
        printfn "Key ** %A **" key

        let state =
            match key, state with
            // Forward
            | ConsoleKey.UpArrow, { Direction = None }
            | ConsoleKey.W, { Direction = None } ->
                { state with CurrentSpeed = speedStep |> percent; Direction = Some Forward }

            | ConsoleKey.UpArrow, { Direction = Some Forward }
            | ConsoleKey.W, { Direction = Some Forward } ->
                { state with CurrentSpeed = state.CurrentSpeed + speedStep |> percent }

            | ConsoleKey.UpArrow, { Direction = Some Backward }
            | ConsoleKey.W, { Direction = Some Backward } ->
                { state with CurrentSpeed = state.CurrentSpeed - speedStep |> percent }

            // Backward
            | ConsoleKey.DownArrow, { Direction = None }
            | ConsoleKey.S, { Direction = None } ->
                { state with CurrentSpeed = speedStep |> percent; Direction = Some Backward }

            | ConsoleKey.DownArrow, { Direction = Some Forward }
            | ConsoleKey.S, { Direction = Some Forward } ->
                { state with CurrentSpeed = state.CurrentSpeed - speedStep |> percent }

            | ConsoleKey.DownArrow, { Direction = Some Backward }
            | ConsoleKey.S, { Direction = Some Backward } ->
                { state with CurrentSpeed = state.CurrentSpeed + speedStep |> percent }

            // Turn left
            | ConsoleKey.LeftArrow, { Direction = Some Forward }
            | ConsoleKey.A, { Direction = Some Forward } ->
                { state with Direction = Some ForwardLeft }

            | ConsoleKey.LeftArrow, { Direction = Some ForwardLeft }
            | ConsoleKey.A, { Direction = Some ForwardLeft } -> state

            | ConsoleKey.LeftArrow, { Direction = Some Backward }
            | ConsoleKey.A, { Direction = Some Backward } ->
                { state with Direction = Some BackwardLeft }

            | ConsoleKey.LeftArrow, { Direction = Some BackwardLeft }
            | ConsoleKey.A, { Direction = Some BackwardLeft } -> state

            | ConsoleKey.LeftArrow, _ | ConsoleKey.A, _ -> { state with Direction = Some Left }

            // Turn right
            | ConsoleKey.RightArrow, { Direction = Some Forward }
            | ConsoleKey.D, { Direction = Some Forward } ->
                { state with Direction = Some ForwardRight }

            | ConsoleKey.RightArrow, { Direction = Some ForwardRight }
            | ConsoleKey.D, { Direction = Some ForwardRight } -> state

            | ConsoleKey.RightArrow, { Direction = Some Backward }
            | ConsoleKey.D, { Direction = Some Backward } ->
                { state with Direction = Some BackwardRight }

            | ConsoleKey.RightArrow, { Direction = Some BackwardRight }
            | ConsoleKey.D, { Direction = Some BackwardRight } -> state

            | ConsoleKey.RightArrow, _ | ConsoleKey.D, _ -> { state with Direction = Some Right }

            // break
            | ConsoleKey.Spacebar, _ -> { state with CurrentSpeed = 0 }
            | _ -> state

        let state =
            match state with
            | { CurrentSpeed = 0; Direction = Some Forward }
            | { CurrentSpeed = 0; Direction = Some Backward } -> { state with Direction = None }
            | _ -> state

        printfn "%A" state

        return! state |> tryKeyboard
    }

[<RequireQualifiedAccess>]
module Transmission =
    let rec private listen (brick: Lego.Ev3.Core.Brick) = async {
        let key = Console.ReadKey().Key
        //printfn "Key ** %A **" key

        let again =
            brick |> listen

        match key with
        | ConsoleKey.UpArrow ->
            printfn "^ Up"
            do! brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Up Ev3Brick.Output.C

            return! again

        | ConsoleKey.DownArrow ->
            printfn "v Down"
            do! brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Down Ev3Brick.Output.C

            return! again

        | ConsoleKey.Spacebar ->
            printfn "Stop!"
            do! brick |> Ev3Brick.stopMotors
            return! again

        | ConsoleKey.Escape ->
            printfn "Exit ..."
            return ()

        | _ ->
            return! again
    }

    let test = async {
        let! brick = Ev3Testing.connect()

        do! brick |> listen
    }

[<EntryPoint>]
let main argv =
    printfn "Start ..."

    configureBrick |> Async.RunSynchronously

    // KeyBoardState.initial |> KeyBoardState.tryKeyboard |> Async.RunSynchronously
    // Transmission.test |> Async.RunSynchronously

    printfn "Done!"
    0
