module Model
open Shared
type RobotMetrics = {
                LengthUpperArm: double
                LengthLowerArm: double
                Radius: double
                CenterX: double
                CenterY: double
                }


type Model = {  Message: string
                Todos: Todo list
                Input: string
                AngleUpperInput: string
                AngleLowerInput: string
                PositionXInput: string
                PositionYInput: string
                AngleUpperOutputF: double
                AngleLowerOutputF: double
                AngleUpperOutput: double
                AngleLowerOutput: double
                Length: double
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
                Metrics: RobotMetrics
                }

type Quadrant =
    | Q1
    | Q2
    | Q3
    | Q4
    | Center
    | AxisOrUnknown


