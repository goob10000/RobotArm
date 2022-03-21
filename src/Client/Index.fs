module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Fulma
open System

type Model = {  Message: string
                Todos: Todo list
                Input: string
                AngleUpperInput: string
                AngleLowerInput: string
                AngleUpperOutputF: double
                AngleLowerOutputF: double
                AngleUpperOutput: double
                AngleLowerOutput: double
                Length: double
                CenterX: double
                CenterY: double
                XPosF1: double
                YPosF1: double
                XPosI1: double
                YPosI1: double
                OldX1: double
                OldY1: double
                XPosF2: double
                YPosF2: double
                XPosI2: double
                YPosI2: double
                OldX2: double
                OldY2: double
                AngleChange: double
                }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | ClearTodo //Defines a type of message that can be sent through the program and the type of message it is
    | SetAngleInputUpper of string
    | SetAngleInputLower of string
    | SetAngleUpper
    | SetAngleLower
    | MoveRobot
    | CalcMovement
    | Tick


let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let centerX = 150.0
    let centerY = 150.0
    let model = {   Todos = []
                    Input = ""
                    AngleUpperInput = ""
                    AngleLowerInput = ""
                    AngleLowerOutputF = 0.0
                    AngleUpperOutputF = 0.0
                    AngleLowerOutput = 0.0
                    AngleUpperOutput = 0.0
                    Message = "Welcome"
                    Length = 100.0
                    CenterX = centerX
                    CenterY = centerY
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

let angleTruncation (v: float) =
    if v > 180.0 then
        System.Console.WriteLine $"v = {v}"
        System.Console.WriteLine $"{floor(v/360.0)}"
        let upAngPreUp = (((v + 180.0) / 360.0) - floor((v+180.0)/360.0))*360.0 - 180.0
        let upAngPre = Math.PI*upAngPreUp/180.0
        System.Console.WriteLine $"UAP = {upAngPreUp}"
        upAngPre
    else if v < -180.0 then
        let upAngPreDown = (((((v* -1.0)+180.0)/360.0) - floor(((v * -1.0)+180.0)/360.0))*360.0 - 180.0)* -1.0
        let upAngPre = Math.PI*upAngPreDown/180.0
        upAngPre
    else
        let upAngPre = Math.PI*v/180.0
        upAngPre

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | ClearTodo -> { model with Todos = [] }, Cmd.none //When the program recieves the message ClearTodo, change the model(world) to match todos with nothing. No further command
    | SetAngleInputUpper value -> { model with AngleUpperInput = value }, Cmd.none
    | SetAngleInputLower value -> { model with AngleLowerInput = value}, Cmd.none
    | SetAngleUpper ->
        match System.Double.TryParse model.AngleUpperInput with
        | true, v ->
            let upAngPre = angleTruncation v
            { model with AngleUpperOutputF = upAngPre; Message = ""}, Cmd.ofMsg CalcMovement
        | false, _ ->
            { model with Message = "Upper Angle Not a Number"}, Cmd.none
    | SetAngleLower ->
        match System.Double.TryParse model.AngleLowerInput with
        | true, v ->
            let lowAngPre = angleTruncation v
            { model with AngleUpperOutputF = lowAngPre; Message = ""}, Cmd.ofMsg CalcMovement
        | false, _ ->
            { model with Message = "Lower Angle Not a Number"}, Cmd.none
    | MoveRobot ->
        let sXPosI1 = (cos(model.AngleUpperOutput) * model.Length) + model.CenterX
        let sYPosI1 = (sin(model.AngleUpperOutput) * model.Length * -1.0) + model.CenterY
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

    | CalcMovement ->
        let XPosF1 = (cos(model.AngleUpperOutputF) * model.Length) + model.CenterX
        let YPosF1 = (sin(model.AngleUpperOutputF) * model.Length * -1.0) + model.CenterY
        let XPosF2 = (cos(model.AngleUpperOutputF + model.AngleLowerOutputF) * model.Length) + XPosF1
        let YPosF2 = (sin(model.AngleUpperOutputF + model.AngleLowerOutputF) * model.Length * -1.0) + YPosF1
        System.Console.WriteLine $"UpperOut -> {model.AngleUpperOutputF}"
        System.Console.WriteLine $"Model Length -> {model.Length}"
        System.Console.WriteLine $"LowerOut -> {model.AngleLowerOutputF}"
        System.Console.WriteLine $"XPosF1 -> %.1f{XPosF1}"
        System.Console.WriteLine $"YPosF1 -> %.1f{YPosF1}"
        System.Console.WriteLine $"XPosF2 -> %.1f{XPosF2}"
        System.Console.WriteLine $"YPosF2 -> %.1f{YPosF2}"
        { model with XPosF1 = XPosF1; YPosF1 = YPosF1; XPosF2 = XPosF2; YPosF2 = YPosF2; Message = $"Move to %.1f{model.XPosF1},%.1f{model.YPosF1} and %.1f{model.XPosF2},%.1f{model.YPosF2}"}, Cmd.none
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
                        prop.onClick (fun _ -> dispatch SetAngleUpper)
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
                        prop.onClick (fun _ -> dispatch SetAngleLower)
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
                        X1 model.CenterY
                        Y1 model.CenterX
                        X2 model.XPosF1
                        Y2 model.YPosF1
                        SVGAttr.StrokeWidth 4
                        SVGAttr.Stroke "lime"
                    ] []
                    line [
                        X1 model.CenterY
                        Y1 model.CenterX
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
