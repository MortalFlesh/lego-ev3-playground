namespace MF.Playground

module KeyboardStateUtils =
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
    open System
    open KeyboardStateUtils

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
