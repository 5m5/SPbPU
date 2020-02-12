import UIKit

let x: Double = 2
let k: Double = 5
let a1: Double = 4
let a2: Double = 6
let b1: Double = 2
let b2: Double = 4

var z1 = 0.0
var z2 = 0.0

var dz1 = 0.0
var dz2 = 0.0

let h = 0.5

var y = 0.0

for i in 1...100 {
    dz1 = z1 + h * z2
    dz2 = z2 + h * (1/(b1 * b2)) * (x - z1 - (b1 + b2) * z2)
    
    y = k * (z1 - a1 * z2 + a2 * z2 + a1 * a2 * dz2)
    
    print("\(i), \(y)")
    
    z1 = dz1
    z2 = dz2
}
