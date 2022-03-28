module Calc

open System
open Model

let sq x = x*x

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


/// convert radians into value between pi and -pi
let angleTruncationRadRad (v: float) =
    v * 180.0 / Math.PI |> angleTruncationDegRad




















let calcTheta (metrics:RobotMetrics) (x:float) (y:float) =
    if x = 0.0 && y = 0.0 then
        let uTheta0 = 0.0
        uTheta0
    else
        let radius = diffSq x y // All of this function converts the X Y and Arm lengths into the proper angles to reach the desired coordinates for the upper arm
        let uAlpha = Math.Acos((metrics.LengthUpperArm * metrics.LengthUpperArm + radius * radius - metrics.LengthLowerArm * metrics.LengthLowerArm)/2.0/radius/metrics.LengthUpperArm)
        let uGamma = Math.Atan(abs(y / x))
        let uTheta = uAlpha + uGamma

        uTheta

let calcPhi (model:RobotMetrics) (x:float) (y:float) =
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
        | AxisOrUnknown -> theta , phi