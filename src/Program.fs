open System
(*
type Controller = Controller of BrandonPotter.XBox.XBoxController

[<RequireQualifiedAccess>]
module Controller =
    type Index =
        | Index of int
        | Any

    [<AutoOpen>]
    module private Controllers =
        open System.Collections.Concurrent

        type private ConnectedControllers = ConcurrentDictionary<Index, Controller>

        let private connectedControllers = ConnectedControllers()

        let addConnected (Controller controller) =
            let index = Index controller.PlayerIndex
            let connectedController = Controller controller

            connectedControllers.AddOrUpdate(index, connectedController, (fun _ _ -> connectedController))
            |> ignore

        let removeConnected index =
            connectedControllers.TryRemove index
            |> ignore

        let tryFind = function
            | Index _ as index ->
                match connectedControllers.TryGetValue index with
                | true, controller -> Some controller
                | _ -> None
            | Any ->
                connectedControllers
                |> Seq.tryHead
                |> Option.map (fun kv -> kv.Value)

        let (|IsConnected|_|) = tryFind

        let controllers () =
            connectedControllers.Values
            |> Seq.toList

        let loadConnectedControllers () =
            BrandonPotter.XBox.XBoxController.GetConnectedControllers()
            |> Seq.iter (Controller >> addConnected)

    loadConnectedControllers()

    let refreshConnectedControllers = loadConnectedControllers
    let connectedControllers = controllers
    let tryFind index = tryFind index

    let rec getAsync index =
        async {
            match index with
            | IsConnected controller -> return controller
            | _ ->
                do! Async.Sleep 1000
                return! getAsync index
        }

    let waitFor index: Async<Controller> =
        // todo - maybe all this body should be in the async{} so the watcher will be disposed after the controller is returned
        loadConnectedControllers()

        use watcher = new BrandonPotter.XBox.XBoxControllerWatcher()

        watcher.add_ControllerConnected(fun controller ->
            let controller = Controller controller
            controller |> addConnected
        )

        watcher.add_ControllerDisconnected(fun controller ->
            Index controller.PlayerIndex |> removeConnected
        )

        getAsync index

    let _registerOnConnect index onConnect =
        use watcher = new BrandonPotter.XBox.XBoxControllerWatcher()

        watcher.add_ControllerConnected(fun controller ->
            let currentIndex = Index controller.PlayerIndex
            let controller = Controller controller

            match index, currentIndex with
            | index, currentIndex when index = currentIndex -> onConnect controller
            | Any, _ -> onConnect controller
            | _ -> ()
        )

        watcher
 *)

type XBoxController = {
    IsConnected: bool
    TriggerLeftPosition: float
    TriggerRightPosition: float
    ThumbLeftX: float
    ThumbLeftY: float
    ThumbRightX: float
    ThumbRightY: float
}

type Controller = Controller of XBoxController

type ThumbStickPositionChanged =
    | X of float
    | Y of float

type TriggerPressed =
    | TriggerPressedPower of float

type PositionChanged =
    | ThumbLeft of ThumbStickPositionChanged
    | ThumbRight of ThumbStickPositionChanged
    | Lt of TriggerPressed
    | Rt of TriggerPressed

module Controller =
    [<Measure>] type private ThumbLX
    [<Measure>] type private ThumbLY
    [<Measure>] type private ThumbRX
    [<Measure>] type private ThumbRY
    [<Measure>] type private LT
    [<Measure>] type private RT

    type private PositionState = {
        ThumbLeftX: float<ThumbLX>
        ThumbLeftY: float<ThumbLY>
        ThumbRightX: float<ThumbRX>
        ThumbRightY: float<ThumbRY>
        LTPosition: float<LT>
        RTPosition: float<RT>
    }

    let private initialPositionState = {
        ThumbLeftX = 50.0<ThumbLX>
        ThumbLeftY = 50.0<ThumbLY>
        ThumbRightX = 50.0<ThumbRX>
        ThumbRightY = 50.0<ThumbRY>
        LTPosition = 0.0<LT>
        RTPosition = 0.0<RT>
    }

    let rec private onPositionChanged waitTime (action: PositionChanged -> unit) (Controller controller) (state: PositionState) = async {
        if controller.IsConnected then
            let currentLtPosition: float<LT> = controller.TriggerLeftPosition * 1.0<LT>
            let currentRtPosition: float<RT> = controller.TriggerRightPosition * 1.0<RT>
            let currentThumbLX: float<ThumbLX> = controller.ThumbLeftX * 1.0<ThumbLX>
            let currentThumbLY: float<ThumbLY> = controller.ThumbLeftY * 1.0<ThumbLY>
            let currentThumbRX: float<ThumbRX> = controller.ThumbRightX * 1.0<ThumbRX>
            let currentThumbRY: float<ThumbRY> = controller.ThumbRightY * 1.0<ThumbRY>

            let state =
                if currentLtPosition <> state.LTPosition then
                    TriggerPressedPower (float currentLtPosition) |> Lt |> action
                    { state with LTPosition = currentLtPosition }
                else state

            let state =
                if currentRtPosition <> state.RTPosition then
                    TriggerPressedPower (float currentRtPosition) |> Rt |> action
                    { state with RTPosition = currentRtPosition }
                else state

            let state =
                if currentThumbLX <> state.ThumbLeftX then
                    X (float currentThumbLX) |> ThumbLeft |> action
                    { state with ThumbLeftX = currentThumbLX }
                else state

            let state =
                if currentThumbLY <> state.ThumbLeftY then
                    Y (float currentThumbLY) |> ThumbLeft |> action
                    { state with ThumbLeftY = currentThumbLY }
                else state

            let state =
                if currentThumbRX <> state.ThumbRightX then
                    X (float currentThumbRX) |> ThumbRight |> action
                    { state with ThumbRightX = currentThumbRX }
                else state

            let state =
                if currentThumbRY <> state.ThumbRightY then
                    Y (float currentThumbRY) |> ThumbRight |> action
                    { state with ThumbRightY = currentThumbRY }
                else state

            do! Async.Sleep waitTime
            return! state |> onPositionChanged waitTime action (Controller controller)
    }

let test = async {
    let a = true
    let b = true
    let c = false
    let d = true

    let mutable i = 3

    while i > 0 do
        i <- i - 1
        printfn "---"

        if a then printfn " A"
        if b then printfn " B"
        if c then printfn " C"
        if d then printfn " D"

    printfn "---"
}

[<EntryPoint>]
let main argv =
    printfn "Running ..."



    test |> Async.RunSynchronously

    (* let controller =
        Controller.Index 1
        |> Controller.waitFor
        |> Async.RunSynchronously
    printfn "Controller 1 is connected (%A)" controller *)

    // problem je v tom, ze se watcher disposne .. :D
    (* use _watcher = Controller._registerOnConnect Controller.Any (fun controller ->
        printfn "Controller 1 is connected (%A)" controller
    ) *)

    (*
    todo:
    - run with connected -> disconnect
    - run with disconnected -> connect -> disconnect
    - run ? -> connect 1 -> connect 2 -> disconnect 1 -> ...
     *)

    printfn "Exiting ... "
    Console.ReadKey() |> ignore

    0
