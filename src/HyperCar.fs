namespace MF.Playground

[<RequireQualifiedAccess>]
module HyperCar =
    open MF.XBoxController

    let configureBrick = async {
        let! brick = Ev3Testing.connect()
        let motorsTimeout = 10000
        let steeringTimeout = 1000

        do!
            Controller.configure
                (function
                    | A -> brick |> Ev3Brick.stopMotors |> Async.Start
                    | B -> brick |> Ev3Brick.makeBeep |> Async.Start
                    | Rb -> brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Down Ev3Brick.Output.C |> Async.Start   // todo - add "memory" to allow only
                    | Lb -> brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Up Ev3Brick.Output.C |> Async.Start     // [1 -> 4] (not cycle directly 4<->1)
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
                            if x > 50 then (x - 50) * -1
                            elif x < 50 then (50 - x)
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
                            steeringTimeout
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
