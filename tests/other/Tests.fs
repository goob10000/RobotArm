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
                Length = 100.0
            }

[<Fact>]
let ``My test`` () =
    Assert.True(true)


[<Fact>]
let ``test Square Function`` () =
    let x = sq 4.0

    Assert.Equal(16.0,x)

[<Fact>]
let ``test Difference of Squares`` () =
    let x = sq 4.0

    Assert.Equal(16.0,x)

[<Fact>]
let ``test Quadrant Function`` () =
    let quadA = quad 4.0 7.0
    let quadB = quad -4.0 6.3
    let quadC = quad -2.7 -9.3
    let quadD = quad 4.0 -6.3
    let quadE = quad 0.0 -17.0
    let quadF = quad 0.0 0.0
    Assert.Equal(Q1,quadA)
    Assert.Equal(Q2, quadB)
    Assert.Equal(Q3, quadC)
    Assert.Equal(Q4, quadD)
    Assert.Equal(Axis, quadE)
    Assert.Equal(Center, quadF)

[<Fact>]
let ``test Quadrant Calc Function`` () =
    let aa = Math.PI/2.0
    let ab = Math.PI/3.0
    let quadCalcA, quadCalcB = quadCalc Q1 aa ab
    Assert.Equal (quadCalcA, Math.PI/2.0, 4)
    Assert.Equal (quadCalcB, Math.PI/3.0, 4)


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
let ``test Opposite Angle Calculator`` () =
    let a = oppAngle 6.0 6.0 6.0
    let b = oppAngle 23.0 17.5 16.2
    let c = oppAngle 3.0 4.0 6.0

    Assert.Equal (Math.PI/3.0, a, 4)
    Assert.Equal (1.50073, b, 4)
    Assert.Equal (0.46049, c, 4)

[<Fact>]
let ``test Upper Angle Calculator`` () =
    let z = calcUpperAngle metrics 100.0 100.0 Q1
    let a = calcUpperAngle metrics -61.6 45.0 Q2
    let b = calcUpperAngle metrics -43.0 -73.0 Q3
    let c = calcUpperAngle metrics 20.0 -15.0 Q4

    Assert.Equal (1.57079, z, 4)
    Assert.Equal (-2.5931, a, 4)
    Assert.Equal (-0.9698, b, 4)
    Assert.Equal (0.80196, c, 4)

[<Fact>]
let ``test Lower Angle Calculator`` () =
    let z = calcLowerAngle metrics 100.0 100.0 Q1
    let a = calcLowerAngle metrics -61.6 45.0 Q2
    let b = calcLowerAngle metrics -43.0 -73.0 Q3
    let c = calcLowerAngle metrics 20.0 -15.0 Q4

    Assert.Equal (-Math.PI/2.0, z, 4)
    Assert.Equal (-2.358903, a, 4)
    Assert.Equal (-2.266719, b, 4)
    Assert.Equal (-2.890936, c, 4)






