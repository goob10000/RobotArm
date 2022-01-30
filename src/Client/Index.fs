module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Fulma
open System

type Model = {  Todos: Todo list
                Input: string
                AngleUpperInput: string
                AngleUpperOutputTest: string
                AngleLowerInput: string
                AngleUpperOutput: double
                AngleLowerOutput: double
                Message: string
                Length: double
                CenterX: double
                CenterY: double
                XPos1: double
                YPos1: double
                OldX1: double
                OldY1: double
                XPos2: double
                YPos2: double
                OldX2: double
                OldY2: double
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
                    AngleLowerOutput = 0.0
                    AngleUpperOutput = 0.0
                    Message = "Welcome"
                    AngleUpperOutputTest = ""
                    Length = 100.0
                    CenterX = centerX
                    CenterY = centerY
                    XPos1 = centerX + 100.0
                    YPos1 = centerY
                    OldX1 = centerX + 100.0
                    OldY1 = centerY
                    XPos2 = centerX + 200.0
                    YPos2 = centerY
                    OldX2 = centerX + 200.0
                    OldY2 = centerY
                    }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

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
            { model with AngleUpperOutput = Math.PI*v/180.0; Message = ""}, Cmd.ofMsg CalcMovement
        | false, _ ->
            { model with Message = "Upper Angle Not a Number"}, Cmd.none
    | SetAngleLower ->
        match System.Double.TryParse model.AngleLowerInput with
        | true, v ->
            { model with AngleLowerOutput = Math.PI*v/180.0; Message = ""}, Cmd.ofMsg CalcMovement
        | false, _ ->
            { model with Message = "Lower Angle Not a Number"}, Cmd.none
    | MoveRobot -> { model with Message = $"Moved to %.1f{model.XPos1},%.1f{model.YPos1} and %.1f{model.XPos2},%.1f{model.YPos2}"; OldX1 = model.XPos1; OldY1 = model.YPos1; OldX2 = model.XPos2; OldY2 = model.YPos2}, Cmd.none
    | CalcMovement ->
        let XPos1 = (cos(model.AngleUpperOutput) * model.Length) + model.CenterX
        let YPos1 = (sin(model.AngleUpperOutput) * model.Length * -1.0) + model.CenterY
        let XPos2 = (cos(model.AngleUpperOutput + model.AngleLowerOutput) * model.Length) + XPos1
        let YPos2 = (sin(model.AngleUpperOutput + model.AngleLowerOutput) * model.Length * -1.0) + YPos1
        System.Console.WriteLine $"UpperOut -> {model.AngleUpperOutput}"
        System.Console.WriteLine $"Model Length -> {model.Length}"
        System.Console.WriteLine $"LowerOut -> {model.AngleLowerOutput}"
        System.Console.WriteLine $"XPos1 -> %.1f{XPos1}"
        System.Console.WriteLine $"YPos1 -> %.1f{YPos1}"
        System.Console.WriteLine $"XPos2 -> %.1f{XPos2}"
        System.Console.WriteLine $"YPos2 -> %.1f{YPos2}"
        { model with XPos1 = XPos1; YPos1 = YPos1; XPos2 = XPos2; YPos2 = YPos2; Message = $"Move to %.1f{model.XPos1},%.1f{model.YPos1} and %.1f{model.XPos2},%.1f{model.YPos2}"}, Cmd.none
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
                    [ Bulma.label model.Message; Bulma.label $"Upper Angle = {model.AngleUpperOutput}"; Bulma.label $"Lower Angle = {model.AngleLowerOutput}"; Bulma.label $"XPos1 = %.1f{model.XPos1}"; Bulma.label $"YPos1 = %.1f{model.YPos1}"; Bulma.label $"XPos2 = %.1f{model.XPos2}"; Bulma.label $"YPos2 = %.1f{model.YPos2}"]
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
                        prop.onClick (fun _ -> dispatch MoveRobot)
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
                        X2 model.XPos1
                        Y2 model.YPos1
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
                        X1 model.XPos1
                        Y1 model.YPos1
                        X2 model.XPos2
                        Y2 model.YPos2
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
