module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Fulma
open System
open Shared
open Model
open Calc



type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | ClearTodo //Defines a type of message that can be sent through the program and the type of message it is
    | SetAngleInputUpper of string
    | SetAngleInputLower of string
    | SetPositionInputX of string
    | SetPositionInputY of string
    | SetAngleUpperA
    | SetAngleLowerA
    | SetAngleUpperP
    | SetAngleLowerP
    | MoveRobot
    | CalcMovementAngle
    | Tick


let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let centerX = 150.0
    let centerY = 150.0
    let lengthUpperArm = 100.0
    let lengthLowerArm = 100.0
    let model = {   Todos = []
                    Input = ""
                    AngleUpperInput = ""
                    AngleLowerInput = ""
                    PositionXInput = ""
                    PositionYInput = ""
                    AngleLowerOutputF = 0.0
                    AngleUpperOutputF = 0.0
                    AngleLowerOutput = 0.0
                    AngleUpperOutput = 0.0
                    Message = "Welcome"
                    Length = 100.0
                    Metrics = {
                        CenterX = 150.0
                        CenterY = 150.0
                        Radius = lengthLowerArm + lengthUpperArm
                        LengthLowerArm = lengthLowerArm
                        LengthUpperArm = lengthUpperArm
                    }
                    XPosF1 = centerX + 100.0
                    YPosF1 = centerY
                    XPosI1 = centerX + 100.0
                    YPosI1 = centerY
                    OldX1 = centerX + 100.0
                    OldY1 = centerY
                    XPosF2 = centerX + 200.0
                    YPosF2 = centerY
                    XPosI2 = centerX + 200.0
                    YPosI2 = centerY
                    OldX2 = centerX + 200.0
                    OldY2 = centerY
                    AngleChange = 10.0 * 2.0 * Math.PI / 360.0
                    }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

(*let sq x = x*x
let diffSq (a: float) (b: float) =
    let diff = sqrt(sq a + sq b)
    diff

let angleTruncationDegRad (v: float) =
    if v > 180.0 then // Subtract/Add 2pi till in the range of the robot arm. Will ensure it takes the correct path as well
        System.Console.WriteLine $"v = {v}"
        System.Console.WriteLine $"{floor(v/360.0)}"
        let upAngPreUp = (((v + 180.0) / 360.0) - floor((v + 180.0) / 360.0)) * 360.0 - 180.0
        let upAngPre = Math.PI*upAngPreUp/180.0
        System.Console.WriteLine $"UAP = {upAngPreUp}"
        upAngPre
    else if v < -180.0 then
        let upAngPreDown = (((((v * -1.0) + 180.0) / 360.0) - floor(((v * -1.0) + 180.0) / 360.0)) * 360.0 - 180.0) * -1.0
        let upAngPre = Math.PI*upAngPreDown/180.0
        upAngPre
    else
        let upAngPre = Math.PI*v/180.0
        upAngPre

let angleTruncationRadRad (v: float) =
    let g = v / 180.0 * Math.PI
    if g > 180.0 then // Subtract/Add 2pi till in the range of the robot arm. Will ensure it takes the correct path as well
        System.Console.WriteLine $"v = {v}"
        System.Console.WriteLine $"{floor(v/360.0)}"
        let upAngPreUp = (((v + 180.0) / 360.0) - floor((v + 180.0) / 360.0)) * 360.0 - 180.0
        let upAngPre = Math.PI*upAngPreUp/180.0
        System.Console.WriteLine $"UAP = {upAngPreUp}"
        upAngPre
    else if g < -180.0 then
        let upAngPreDown = (((((v * -1.0) + 180.0) / 360.0) - floor(((v * -1.0) + 180.0) / 360.0)) * 360.0 - 180.0) * -1.0
        let upAngPre = Math.PI*upAngPreDown/180.0
        upAngPre
    else
        v

let calcTheta (model:Model) (x:float) (y:float) =
    if x = 0.0 && y = 0.0 then
        let uTheta0 = 0.0
        uTheta0
    else
        let radius = diffSq x y // All of this function converts the X Y and Arm lengths into the proper angles to reach the desired coordinates for the upper arm
        let uAlpha = Math.Acos((model.LengthUpperArm * model.LengthUpperArm + radius * radius - model.LengthLowerArm * model.LengthLowerArm)/2.0/radius/model.LengthUpperArm)
        let uGamma = Math.Atan(abs(y / x))
        let uTheta = uAlpha + uGamma

        uTheta

let calcPhi (model:Model) (x:float) (y:float) =
    if x = 0.0 && y = 0.0 then
        let uPhi0 = Math.PI
        uPhi0
    else
        let radius = diffSq x y // All of this function converts the X Y and Arm lengths into the proper angles to reach the desired coordinates for the lower arm
        let uBeta = Math.Acos((model.LengthLowerArm * model.LengthLowerArm + radius * radius - model.LengthUpperArm * model.LengthUpperArm)/2.0/radius/model.LengthLowerArm)
        let uDelta = Math.Atan(abs (x / y))
        let uPhi = Math.PI / 2.0 - uBeta - uDelta
        uPhi

let quad (x:float) (y:float) =
    if x = 0.0 && y = 0.0 then
        Center
    else if x > 0.0 && y >= 0.0 then
        Q1
    else if x <= 0.0 && y > 0.0 then
        Q2
    else if x < 0.0 && y <= 0.0 then
        Q3
    else if x >= 0.0 && y < 0.0 then
        Q4
    else
        AxisOrUnknown

let quadCalc (q:Quadrant) (theta:float) (phi:float) =
    match q with
        | Q1 -> theta , phi
        | Q2 -> Math.PI - theta , Math.PI - phi
        | Q3 -> -1.0 * (Math.PI - theta) , -1.0 * (Math.PI - phi)
        | Q4 -> -1.0 * theta , -1.0 * phi
        | Center -> theta , phi
        | AxisOrUnknown -> theta , phi *)


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | ClearTodo -> { model with Todos = [] }, Cmd.none //When the program recieves the message ClearTodo, change the model(world) to match todos with nothing. No further command
    | SetAngleInputUpper value -> { model with AngleUpperInput = value }, Cmd.none
    | SetAngleInputLower value -> { model with AngleLowerInput = value}, Cmd.none
    | SetPositionInputX value -> { model with PositionXInput = value}, Cmd.none
    | SetPositionInputY value -> { model with PositionYInput = value}, Cmd.none


    | SetAngleUpperP ->
        match System.Double.TryParse model.PositionXInput with //Check to make sure they are actually numbers
        | true, x ->
            match System.Double.TryParse model.PositionYInput with
            | true, y ->
            if model.Metrics.Radius >= diffSq x y then //Check to make sure not out of range of robot arm
                let uTheta = calcTheta model.Metrics x y
                let uPhi = calcPhi model.Metrics x y
                let quad = quad x y
                System.Console.WriteLine $"uTheta -> {uTheta}"
                System.Console.WriteLine $"uPhi -> {uPhi}"
                let iTheta , iPhi = quadCalc quad uTheta uPhi
                let fTheta = angleTruncationRadRad iTheta
                let fPhi = angleTruncationRadRad iPhi
                let upperAngle = fTheta
                System.Console.WriteLine "SetAngleUpperP"
                System.Console.WriteLine $"upperAngle -> {upperAngle}"
                System.Console.WriteLine $"quad -> {quad}"
                System.Console.WriteLine $"iTheta,iPhi -> {iTheta},{iPhi}"

                { model with AngleUpperOutputF = upperAngle; Message = ""}, Cmd.ofMsg CalcMovementAngle
            else
                {model with Message = "Too Far"}, Cmd.none
            | false, _ ->
                {model with Message = "Not A Possible Y Position"}, Cmd.none
        | false, _ ->
            {model with Message = "Not A Possible X Position"}, Cmd.none


    | SetAngleLowerP ->
        match System.Double.TryParse model.PositionXInput with
        | true, x ->
            match System.Double.TryParse model.PositionYInput with
            | true, y ->
            if model.Metrics.Radius >= diffSq x y then
                let uTheta = calcTheta model.Metrics x y
                let uPhi = calcPhi model.Metrics x y
                let quad = quad x y
                let iTheta , iPhi = quadCalc quad uTheta uPhi
                let fTheta = angleTruncationRadRad iTheta
                let fPhi = angleTruncationRadRad iPhi
                let lowerAngle = fTheta - fPhi
                { model with AngleLowerOutputF = lowerAngle; Message = ""}, Cmd.ofMsg CalcMovementAngle
            else
                {model with Message = "Too Far"}, Cmd.none
            | false, _ ->
                {model with Message = "Not A Possible Y Position"}, Cmd.none
        | false, _ ->
            {model with Message = "Not A Possible X Position"}, Cmd.none

    | CalcMovementAngle ->
        let XPosF1 = (cos(model.AngleUpperOutputF) * model.Length) + model.Metrics.CenterX
        let YPosF1 = (sin(model.AngleUpperOutputF) * model.Length * -1.0) + model.Metrics.CenterY
        let XPosF2 = (cos(model.AngleUpperOutputF + model.AngleLowerOutputF) * model.Length) + XPosF1
        let YPosF2 = (sin(model.AngleUpperOutputF + model.AngleLowerOutputF) * model.Length * -1.0) + YPosF1
        // System.Console.WriteLine $"UpperOut -> {model.AngleUpperOutputF}"
        // System.Console.WriteLine $"Model Length -> {model.Length}"
        // System.Console.WriteLine $"LowerOut -> {model.AngleLowerOutputF}"
        // System.Console.WriteLine $"XPosF1 -> %.1f{XPosF1}"
        // System.Console.WriteLine $"YPosF1 -> %.1f{YPosF1}"
        // System.Console.WriteLine $"XPosF2 -> %.1f{XPosF2}"
        // System.Console.WriteLine $"YPosF2 -> %.1f{YPosF2}"
        { model with XPosF1 = XPosF1; YPosF1 = YPosF1; XPosF2 = XPosF2; YPosF2 = YPosF2; Message = $"Move to %.1f{model.XPosF1},%.1f{model.YPosF1} and %.1f{model.XPosF2},%.1f{model.YPosF2}"}, Cmd.none

    | SetAngleUpperA ->
        match System.Double.TryParse model.AngleUpperInput with
        | true, v ->
            let upAngPre = angleTruncationDegRad v
            { model with AngleUpperOutputF = upAngPre; Message = ""}, Cmd.ofMsg CalcMovementAngle
        | false, _ ->
            { model with Message = "Upper Angle Not a Number"}, Cmd.none
    | SetAngleLowerA ->
        match System.Double.TryParse model.AngleLowerInput with
        | true, v ->
            let lowAngPre = angleTruncationDegRad v
            { model with AngleLowerOutputF = lowAngPre; Message = ""}, Cmd.ofMsg CalcMovementAngle
        | false, _ ->
            { model with Message = "Lower Angle Not a Number"}, Cmd.none
    | MoveRobot ->
        let sXPosI1 = (cos(model.AngleUpperOutput) * model.Length) + model.Metrics.CenterX
        let sYPosI1 = (sin(model.AngleUpperOutput) * model.Length * -1.0) + model.Metrics.CenterY
        let sXPosI2 = (cos(model.AngleUpperOutput + model.AngleLowerOutput) * model.Length) + sXPosI1
        let sYPosI2 = (sin(model.AngleUpperOutput + model.AngleLowerOutput) * model.Length * -1.0) + sYPosI1

        { model with Message = $"Moved to %.1f{sXPosI1},%.1f{sYPosI1} and %.1f{sXPosI2},%.1f{sYPosI2}"; OldX1 = sXPosI1; OldY1 = sYPosI1; OldX2 = sXPosI2; OldY2 = sYPosI2}, Cmd.ofMsg Tick
        //model, Cmd.ofMsg Tick
    | Tick ->
        if  abs (model.AngleLowerOutputF - model.AngleLowerOutput) < 0.01 && abs (model.AngleUpperOutputF - model.AngleUpperOutput) > 0.01 then
            if (model.AngleUpperOutputF - model.AngleUpperOutput) > 0.0 then
                let UpperAngleDif = min model.AngleChange (model.AngleUpperOutputF - model.AngleUpperOutput)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs UpperAngleDif < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleUpperOutput = model.AngleUpperOutput + UpperAngleDif}, Cmd.OfAsync.result tick
            else if (model.AngleUpperOutputF - model.AngleUpperOutput) < 0.0 then
                let UpperAngleDif = min model.AngleChange ((model.AngleUpperOutputF - model.AngleUpperOutput) * -1.0)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs UpperAngleDif < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleUpperOutput = model.AngleUpperOutput - UpperAngleDif}, Cmd.OfAsync.result tick
            else
                model,Cmd.none
        else if  abs (model.AngleUpperOutputF - model.AngleUpperOutput) < 0.01 && abs (model.AngleLowerOutputF - model.AngleLowerOutput) > 0.01 then
            if (model.AngleLowerOutputF - model.AngleLowerOutput) > 0.0 then
                let LowerAngleDif = min model.AngleChange (model.AngleLowerOutputF - model.AngleLowerOutput)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs LowerAngleDif < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput + LowerAngleDif}, Cmd.OfAsync.result tick
            else if (model.AngleLowerOutputF - model.AngleLowerOutput) < 0.0 then
                let LowerAngleDif = min model.AngleChange ((model.AngleLowerOutputF - model.AngleLowerOutput) * -1.0)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs LowerAngleDif < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput - LowerAngleDif}, Cmd.OfAsync.result tick
            else
                model,Cmd.none
        else if  abs (model.AngleUpperOutputF - model.AngleUpperOutput) > 0.01 && abs (model.AngleLowerOutputF - model.AngleLowerOutput) > 0.01 then
            if (model.AngleLowerOutputF - model.AngleLowerOutput) > 0.0 && (model.AngleUpperOutputF - model.AngleUpperOutput) > 0.0 then
                let LowerAngleDif = min model.AngleChange (model.AngleLowerOutputF - model.AngleLowerOutput)
                let UpperAngleDif = min model.AngleChange (model.AngleUpperOutputF - model.AngleUpperOutput)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs (LowerAngleDif) < 0.01 && abs (UpperAngleDif) < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput + LowerAngleDif; AngleUpperOutput = model.AngleUpperOutput + UpperAngleDif}, Cmd.OfAsync.result tick
            else if (model.AngleLowerOutputF - model.AngleLowerOutput) < 0.0 && (model.AngleUpperOutputF - model.AngleUpperOutput) > 0.0 then
                let LowerAngleDif = min model.AngleChange ((model.AngleLowerOutputF - model.AngleLowerOutput) * -1.0)
                let UpperAngleDif = min model.AngleChange (model.AngleUpperOutputF - model.AngleUpperOutput)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs (LowerAngleDif) < 0.01 && abs (UpperAngleDif) < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput - LowerAngleDif; AngleUpperOutput = model.AngleUpperOutput + UpperAngleDif}, Cmd.OfAsync.result tick
            else if (model.AngleLowerOutputF - model.AngleLowerOutput) > 0.0 && (model.AngleUpperOutputF - model.AngleUpperOutput) < 0.0 then
                let LowerAngleDif = min model.AngleChange (model.AngleLowerOutputF - model.AngleLowerOutput)
                let UpperAngleDif = min model.AngleChange ((model.AngleUpperOutputF - model.AngleUpperOutput) * -1.0)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs (LowerAngleDif) < 0.01 && abs (UpperAngleDif) < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput + LowerAngleDif; AngleUpperOutput = model.AngleUpperOutput - UpperAngleDif}, Cmd.OfAsync.result tick
            else if (model.AngleLowerOutputF - model.AngleLowerOutput) < 0.0 && (model.AngleUpperOutputF - model.AngleUpperOutput) < 0.0 then
                let LowerAngleDif = min model.AngleChange ((model.AngleLowerOutputF - model.AngleLowerOutput) * -1.0)
                let UpperAngleDif = min model.AngleChange ((model.AngleUpperOutputF - model.AngleUpperOutput) * -1.0)
                //let yUpperAngleDif = min model.AngleChange (model.YPosF1 - model.OldY1)
                if abs (LowerAngleDif) < 0.01 && abs (UpperAngleDif) < 0.01 then
                    model,Cmd.none
                else
                    let tick =
                        async {
                            do! Async.Sleep 50
                            return MoveRobot
                        }
                    {model with AngleLowerOutput = model.AngleLowerOutput - LowerAngleDif; AngleUpperOutput = model.AngleUpperOutput - UpperAngleDif}, Cmd.OfAsync.result tick
            else
                model,Cmd.none
        else
            model,Cmd.none

    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none

open Feliz
open Feliz.Bulma

open Fable.React.Standard
open Fable.React.Props
open Fable.React.Helpers

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            // Html.ol [
            //     for todo in model.Todos do
            //         Html.li [ prop.text todo.Description ]
            // ]
            Fulma.Message.message []
                [ Fulma.Message.body [ Fulma.Common.GenericOption.Modifiers [   Modifier.TextAlignment (Fulma.Screen.All, Fulma.TextAlignment.Centered)
                                                                                Fulma.Modifier.BackgroundColor Fulma.Color.IsGreyLighter
                                                                                Fulma.Modifier.TextColor Fulma.Color.IsLink
                                                                                Fulma.Modifier.TextWeight Fulma.TextWeight.Bold ] ]
                    [ Bulma.label model.Message; Bulma.label $"Upper Angle Forecast = {model.AngleUpperOutputF}"; Bulma.label $"Lower Angle Forecast = {model.AngleLowerOutputF}"; Bulma.label $"Upper Angle = {model.AngleUpperOutput}"; Bulma.label $"Lower Angle = {model.AngleLowerOutput}";(* Bulma.label $"XPosF1 = %.1f{model.XPosF1}"; Bulma.label $"YPosF1 = %.1f{model.YPosF1}"; Bulma.label $"XPosF2 = %.1f{model.XPosF2}"; Bulma.label $"YPosF2 = %.1f{model.YPosF2}"*)]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                (*if model.Todos.Length < 500 then
                    Bulma.control.p [
                        control.isExpanded
                        prop.children [
                            Bulma.input.text [
                                prop.value model.Input
                                prop.placeholder "What needs to be done?"
                                prop.onChange (fun x -> SetInput x |> dispatch)
                            ]
                        ]
                    ]
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isPrimary
                            prop.disabled ((model.Angle <> "" || model.Input <> "" )|> not)
                            prop.onClick (fun _ -> dispatch AddTodo)
                            prop.text "Add"
                        ]
                    ]*)
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.AngleUpperInput
                            prop.placeholder "AngleU"
                            prop.onChange (fun x -> SetAngleInputUpper x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled ((model.AngleUpperInput <> "" || model.Input <> "" )|> not)
                        prop.onClick (fun _ -> dispatch SetAngleUpperA)
                        prop.text "Set UAng"
                    ]
                ]
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.width 400.0
                            prop.value model.AngleLowerInput
                            prop.placeholder "AngleL"
                            prop.onChange (fun x -> SetAngleInputLower x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled ((model.AngleLowerInput <> "" || model.Input <> "" )|> not)
                        prop.onClick (fun _ -> dispatch SetAngleLowerA)
                        prop.text "Set LAng"
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isInfo
                        prop.onClick (fun _ -> dispatch Tick)
                        prop.text "Move Line"
                    ]
                ]
                (*Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled ((model.Angle <> "" || model.Input <> "" )|> not) //Button is disabled unless there's text in either text box
                        prop.onClick (fun _ -> dispatch ClearTodo) //Function sends negligable information to the program and sends message(dispatch) ClearTodo
                        prop.text "Clear Todo" //Whats in the button
                    ]
                ]*)
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                (*if model.Todos.Length < 500 then
                    Bulma.control.p [
                        control.isExpanded
                        prop.children [
                            Bulma.input.text [
                                prop.value model.Input
                                prop.placeholder "What needs to be done?"
                                prop.onChange (fun x -> SetInput x |> dispatch)
                            ]
                        ]
                    ]
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isPrimary
                            prop.disabled ((model.Angle <> "" || model.Input <> "" )|> not)
                            prop.onClick (fun _ -> dispatch AddTodo)
                            prop.text "Add"
                        ]
                    ]*)
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.PositionXInput
                            prop.placeholder "X"
                            prop.onChange (fun x -> SetPositionInputX x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.width 400.0
                            prop.value model.PositionYInput
                            prop.placeholder "Y"
                            prop.onChange (fun x -> SetPositionInputY x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled ((model.PositionYInput <> "" || model.Input <> "" )|> not)
                        prop.onClick (fun _ -> dispatch SetAngleLowerP; dispatch SetAngleUpperP)
                        prop.text "Set Position"
                    ]
                ]
                (*Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled ((model.Angle <> "" || model.Input <> "" )|> not) //Button is disabled unless there's text in either text box
                        prop.onClick (fun _ -> dispatch ClearTodo) //Function sends negligable information to the program and sends message(dispatch) ClearTodo
                        prop.text "Clear Todo" //Whats in the button
                    ]
                ]*)
            ]
        ]
    ]

let drawingArea (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.field.div [
            field.isGrouped
            prop.children [
                svg [SVGAttr.Width 1500.0
                     SVGAttr.Height 750.0] [
                    rect [
                    X 0
                    Y 0
                    SVGAttr.Width "100%"
                    SVGAttr.Height "100%"
                    SVGAttr.StrokeWidth 1
                    SVGAttr.Fill "purple"
                    ] []
                    circle [
                        Cx 10
                        Cy 100
                        R 0
                        SVGAttr.StrokeWidth 1
                        SVGAttr.Fill "red"
                    ] []
                    line [
                        X1 model.Metrics.CenterY
                        Y1 model.Metrics.CenterX
                        X2 model.XPosF1
                        Y2 model.YPosF1
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Stroke "lime"
                    ] []
                    line [
                        X1 model.Metrics.CenterY
                        Y1 model.Metrics.CenterX
                        X2 model.OldX1
                        Y2 model.OldY1
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Stroke "blue"
                    ] []
                    line [
                        X1 model.XPosF1
                        Y1 model.YPosF1
                        X2 model.XPosF2
                        Y2 model.YPosF2
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Stroke "lime"
                    ] []
                    line [
                        X1 model.OldX1
                        Y1 model.OldY1
                        X2 model.OldX2
                        Y2 model.OldY2
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Stroke "red"
                    ] []
                ]
                Fable.React.Standard.svg []
                    [rect [
                        Cx "0"
                        Cy "0"
                        Rx "10"
                        Ry "10"
                        ]
                        []
                    ]
                (*Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Set X Value"
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch ClearTodo)
                        prop.text "Clear Todo"
                    ]
                ]*)
            ]
        ]
    ]
let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            Feliz.style.backgroundSize "cover"
            Feliz.style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            Feliz.style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                Bulma.text.hasTextCentered
                                prop.text "Robot Arm"
                            ]
                            containerBox model dispatch //Calls function to draw boxes and buttons
                            drawingArea model dispatch //Calls function for drawing area

                        ]
                    ]
                ]
            ]
        ]
    ]
