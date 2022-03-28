module Tests

open System
open Xunit
open Calc
open Model

let centerX = 150.0
let centerY = 150.0
let lengthUpperArm = 100.0
let lengthLowerArm = 100.0
let metrics = {
                CenterX = 150.0
                CenterY = 150.0
                Radius = lengthLowerArm + lengthUpperArm
                LengthLowerArm = lengthLowerArm
                LengthUpperArm = lengthUpperArm
            }

[<Fact>]
let ``My test`` () =
    Assert.True(true)


[<Fact>]
let ``test square`` () =
    let x = sq 4.0

    Assert.Equal(16.0,x)

[<Fact>]
let ``test Angle Truncation Deg to Rad`` () =
    let deg123 = angleTruncationDegRad 123.0
    let degneg293 = angleTruncationDegRad -293.0
    let deglong = angleTruncationDegRad 2309500.0
    let deg7pi = angleTruncationDegRad 1260.0

    Assert.Equal (2.14675, deg123, 4)
    Assert.Equal (1.16937, degneg293, 4)
    Assert.Equal (1.74532, deglong, 4)
    Assert.Equal (-1.0 * Math.PI, deg7pi, 4)

[<Fact>]
let ``test Angle Truncation Rad to Rad`` () =
    let a = angleTruncationRadRad Math.PI / 3.0
    let b = angleTruncationRadRad (Math.PI * 7.0)
    let c = angleTruncationRadRad (Math.PI * -1.5)

    Assert.Equal (Math.PI / 3.0, a, 4)
    Assert.Equal (-1.0 * Math.PI, b, 4)
    Assert.Equal (Math.PI / 2.0, c, 4)

[<Fact>]
let ``test Theta Calculator`` () =
    let a = calcTheta metrics -61.6 45.0
    let b = calcTheta metrics 20.0 -15.0
    let c = calcTheta metrics -43.0 73.0

    Assert.Equal (1.81036, a, 4)
    Assert.Equal (2.08896, b, 4)
    Assert.Equal (2.17183, c, 4)

[<Fact>]
let ``test Phi Calculator`` () =
    let a = calcPhi metrics -61.6 45.0
    let b = calcPhi metrics 20.0 -15.0
    let c = calcPhi metrics -43.0 73.0

    Assert.Equal (-0.548536, a, 4)
    Assert.Equal (-0.80196, b, 4)
    Assert.Equal (-0.09488, c, 4)






