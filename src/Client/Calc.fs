module Calc

open System
open Model

let sq x = x*x

let diffSq (a: float) (b: float) =
    let diff = abs(sqrt(sq a + sq b))
    diff

let oppAngle (sL1:float) (sL2:float) (sL3:float) =
    let output = Math.Acos((sq(sL2) + sq(sL3) - sq(sL1))/2.0/sL2/sL3)
    output

let angleTruncationDegRad (v: float) =
    if v > 180.0 then // Subtract/Add 2pi till in the range of the robot arm. Will ensure it takes the correct path as well
        //System.Console.WriteLine $"v = {v}"
        //System.Console.WriteLine $"{floor(v/360.0)}"
        let upAngPreUp = (((v + 180.0) / 360.0) - floor((v + 180.0) / 360.0)) * 360.0 - 180.0
        let upAngPre = Math.PI*upAngPreUp/180.0
        //System.Console.WriteLine $"UAP = {upAngPreUp}"
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
    (v * 180.0 / Math.PI) |> angleTruncationDegRad

let calcGamma (x:float) (y:float) (q:Quadrant) =
    match q with
        | Q1 ->
            let output = Math.Atan(abs(y / x))
            output
        | Q2 ->
            let output = Math.Atan(abs(x / y))
            output
        | Q3 ->
            let output = Math.Atan(abs(y / x))
            output
        | Q4 ->
            let output = Math.Atan(abs(x / y))
            output
        | Axis ->
            let output = Math.Atan(abs(x / y))
            output
        | Center ->
            0.0
        | Unknown ->
            0.0

let calcDelta (x:float) (y:float) (q:Quadrant) =
    match q with
        | Q1 ->
            let output = Math.Atan(abs(x / y))
            output
        | Q2 ->
            let output = Math.Atan(abs(y / x))
            output
        | Q3 ->
            let output = Math.Atan(abs(x / y))
            output
        | Q4 ->
            let output = Math.Atan(abs(y / x))
            output
        | Axis ->
            let output = Math.Atan(abs(x / y))
            output
        | Center ->
            0.0
        | Unknown ->
            0.0

let calcUpperAngle (metrics:RobotMetrics) (x:float) (y:float) (q:Quadrant) =
    let radius = diffSq x y  // All of this function converts the X Y and Arm lengths into the proper angles to reach the desired coordinates for the upper arm
    match q with
        | Q1 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q1
            let uTheta = uAlpha + uGamma
            let uAngle = angleTruncationRadRad uTheta
            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uAngle -> {uAngle}"

            uAngle

        | Q2 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q2
            let uTheta = uAlpha + uGamma
            let uAngle = angleTruncationRadRad (uTheta + (Math.PI/2.0))
            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uAngle -> {uAngle}"

            uAngle

        | Q3 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q3
            let uTheta = uAlpha + uGamma
            let uAngle = angleTruncationRadRad (-Math.PI + uTheta)
            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uAngle -> {uAngle}"

            uAngle
        | Q4 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q4
            let uTheta = uAlpha + uGamma
            let uAngle = angleTruncationRadRad (-Math.PI/2.0 + uTheta)
            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uAngle -> {uAngle}"

            uAngle
        | Axis ->
            if x > 0.0 then
                let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
                let uGamma = calcGamma x y Axis
                let uTheta = uAlpha + uGamma
                let uAngle = angleTruncationRadRad (uTheta + (Math.PI/2.0))
                System.Console.WriteLine $"uAlpha -> {uAlpha}"
                System.Console.WriteLine $"uGamma -> {uGamma}"
                System.Console.WriteLine $"uTheta -> {uTheta}"
                System.Console.WriteLine $"uAngle -> {uAngle}"

                uAngle

            else if x < 0.0 then
                let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
                let uGamma = calcGamma x y Axis
                let uTheta = uAlpha + uGamma
                let uAngle = angleTruncationRadRad (-Math.PI/2.0 + uTheta)
                System.Console.WriteLine $"uAlpha -> {uAlpha}"
                System.Console.WriteLine $"uGamma -> {uGamma}"
                System.Console.WriteLine $"uTheta -> {uTheta}"
                System.Console.WriteLine $"uAngle -> {uAngle}"

                uAngle

            else if y > 0.0 then
                let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
                let uGamma = calcGamma x y Axis
                let uTheta = uAlpha + uGamma
                let uAngle = angleTruncationRadRad uTheta
                System.Console.WriteLine $"uAlpha -> {uAlpha}"
                System.Console.WriteLine $"uGamma -> {uGamma}"
                System.Console.WriteLine $"uTheta -> {uTheta}"
                System.Console.WriteLine $"uAngle -> {uAngle}"

                uAngle

            else if y < 0.0 then
                let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
                let uGamma = calcGamma x y Axis
                let uTheta = uAlpha + uGamma
                let uAngle = angleTruncationRadRad (-Math.PI + uTheta)
                System.Console.WriteLine $"uAlpha -> {uAlpha}"
                System.Console.WriteLine $"uGamma -> {uGamma}"
                System.Console.WriteLine $"uTheta -> {uTheta}"
                System.Console.WriteLine $"uAngle -> {uAngle}"

                uAngle
            else
                0.0
        | Center ->
            let uTheta0 = 0.0
            uTheta0
        | Unknown ->
            0.0


let calcLowerAngle (metrics:RobotMetrics) (x:float) (y:float) (q:Quadrant) =
    let radius = diffSq x y // All of this function converts the X Y and Arm lengths into the proper angles to reach the desired coordinates for the upper arm
    match q with
        | Q1 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q1
            let uTheta = uAlpha + uGamma
            let uBeta = oppAngle metrics.LengthUpperArm metrics.LengthLowerArm radius
            let uDelta = calcDelta x y Q1
            let uPhi = uBeta + uDelta - Math.PI/2.0
            let lAngle = -uTheta - uPhi

            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uBeta -> {uBeta}"
            System.Console.WriteLine $"uDelta -> {uDelta}"
            System.Console.WriteLine $"uPhi -> {uPhi}"
            System.Console.WriteLine $"lAngle -> {lAngle}"

            lAngle

        | Q2 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q2
            let uTheta = uAlpha + uGamma
            let uBeta = oppAngle metrics.LengthUpperArm metrics.LengthLowerArm radius
            let uDelta = calcDelta x y Q2
            let uPhi = uBeta + uDelta - Math.PI/2.0
            let lAngle = -uTheta - uPhi

            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uBeta -> {uBeta}"
            System.Console.WriteLine $"uDelta -> {uDelta}"
            System.Console.WriteLine $"uPhi -> {uPhi}"
            System.Console.WriteLine $"lAngle -> {lAngle}"

            lAngle

        | Q3 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q3
            let uTheta = uAlpha + uGamma
            let uBeta = oppAngle metrics.LengthUpperArm metrics.LengthLowerArm radius
            let uDelta = calcDelta x y Q3
            let uPhi = uBeta + uDelta - Math.PI/2.0
            let lAngle = -uTheta - uPhi

            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uBeta -> {uBeta}"
            System.Console.WriteLine $"uDelta -> {uDelta}"
            System.Console.WriteLine $"uPhi -> {uPhi}"
            System.Console.WriteLine $"lAngle -> {lAngle}"

            lAngle

        | Q4 ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Q4
            let uTheta = uAlpha + uGamma
            let uBeta = oppAngle metrics.LengthUpperArm metrics.LengthLowerArm radius
            let uDelta = calcDelta x y Q4
            let uPhi = uBeta + uDelta - Math.PI/2.0
            let lAngle = -uTheta - uPhi

            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uBeta -> {uBeta}"
            System.Console.WriteLine $"uDelta -> {uDelta}"
            System.Console.WriteLine $"uPhi -> {uPhi}"
            System.Console.WriteLine $"lAngle -> {lAngle}"

            lAngle

        | Axis ->
            let uAlpha = oppAngle metrics.LengthLowerArm metrics.LengthUpperArm radius
            let uGamma = calcGamma x y Axis
            let uTheta = uAlpha + uGamma
            let uBeta = oppAngle metrics.LengthUpperArm metrics.LengthLowerArm radius
            let uDelta = calcDelta x y Axis
            let uPhi = uBeta + uDelta - Math.PI/2.0
            let lAngle = -uTheta - uPhi

            System.Console.WriteLine $"uAlpha -> {uAlpha}"
            System.Console.WriteLine $"uGamma -> {uGamma}"
            System.Console.WriteLine $"uTheta -> {uTheta}"
            System.Console.WriteLine $"uBeta -> {uBeta}"
            System.Console.WriteLine $"uDelta -> {uDelta}"
            System.Console.WriteLine $"uPhi -> {uPhi}"
            System.Console.WriteLine $"lAngle -> {lAngle}"

            lAngle

        | Center ->
            let uTheta0 = Math.PI

            uTheta0

        | Unknown ->
            0.0


//Includes Axis on counterxlockwisemost side of Quadrant
let quad (x:float) (y:float) =
    if x = 0.0 && y = 0.0 then
        Center
    else if x > 0.0 && y > 0.0 then
        Q1
    else if x < 0.0 && y > 0.0 then
        Q2
    else if x < 0.0 && y < 0.0 then
        Q3
    else if x > 0.0 && y < 0.0 then
        Q4
    else if x = 0.0 || y = 0.0 then
        Axis
    else
        Unknown

let quadCalc (q:Quadrant) (theta:float) (phi:float) =
    match q with
        | Q1 -> theta , phi
        | Q2 -> Math.PI - phi , -theta
        | Q3 -> -Math.PI + phi, Math.PI - theta
        | Q4 -> -phi , -Math.PI + theta
        | Axis -> theta, phi
        | Center -> theta , phi
        | Unknown -> theta , phi
