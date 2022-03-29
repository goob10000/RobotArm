open System

let rD (f:float) =
    f/Math.PI*180.0

type Quadrant =
| Q1
| Q2
| Q3
| Q4
| Center
| Unknown


let sq x = x*x

let oppAngle (sL1:float) (sL2:float) (sL3:float) =
    let output = Math.Acos((sq(sL2) + sq(sL3) - sq(sL1))/(2.0*sL2*sL3))
    output

let x = 80.0
let y = 120.0
let l1 = 100.0
let l2 = 100.0
let r = sqrt(x*x + y*y)
let gamma = Math.Atan(y/x)
let alpha = Math.Acos((l1*l1+r*r-l2*l2)/2.0/l1/r)
let beta = Math.Acos((l2*l2+r*r-l1*l1)/2.0/l2/r)
let theta = gamma + alpha
let delta = 90.0 - gamma
let phi = 90.0 - beta - delta
let thetaDegrees = rD theta
let phiDegrees = rD phi
let alphaDegrees = rD alpha
let betaDegrees = rD beta
let upperAngle = thetaDegrees
let lowerAngle = alphaDegrees - phiDegrees
let legal = l1 + l2 > r

sign -6.3
let a = Math.Acos 0.0
let b = Math.Acos -0.0
let c = Math.Acos
let d = Math.Acos

let e = oppAngle 23.0 17.5 16.2

