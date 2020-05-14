namespace MF.Playground

[<RequireQualifiedAccess>]
module Steering =
    open System

    let rec private listen (brick: Lego.Ev3.Core.Brick) = async {
        let key = Console.ReadKey().Key
        //printfn "Key ** %A **" key

        let again =
            brick |> listen

        match key with
        | ConsoleKey.LeftArrow ->
            printfn "< Left"
            //do! brick |> Ev3Brick.startMotors Ev3Brick.Output.D 50 1
            do! brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Up Ev3Brick.Output.D

            return! again

        | ConsoleKey.RightArrow ->
            printfn "> Right"
            //do! brick |> Ev3Brick.startMotors Ev3Brick.Output.D 50 1
            do! brick |> Ev3Brick.turnMotor90 Ev3Brick.Direction.Down Ev3Brick.Output.D

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
