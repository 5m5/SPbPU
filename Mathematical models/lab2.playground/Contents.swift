import UIKit

var array = [Float]()

for _ in 0...100 {
    let z = sqrt(-2 * log(Double.random(in: 0...1))) * cos(2 * Double.pi * Double.random(in: 0...1))
    let g = z * 1 + 0
    array.append(Float(g))
}

print(array)

var numbersFrom0to1 = 0
var numbersFrom1to2 = 0
var numbersFrom2to3 = 0
var numbersFrom3to4 = 0
var numbersFrom4to5 = 0
var numbersFrom5to6 = 0
var numbersFrom6to7 = 0
var numbersFrom7to8 = 0
var numbersFrom8to9 = 0
var numbersFrom9to10 = 0

var x1 = 0
var x2 = 0
var x3 = 0
var x4 = 0
var x5 = 0
var x6 = 0

for number in array {
    if number >= -9.44 && number <= -8.42 {
        numbersFrom0to1 += 1
    } else if number >= -8.42 && number <= -7.4 {
        numbersFrom1to2 += 1
    } else if number >= -7.4 && number <= -6.38 {
        numbersFrom2to3 += 1
    } else if number >= -6.38 && number <= -5.36 {
        numbersFrom3to4 += 1
    } else if number >= -5.36 && number <= -4.34 {
        numbersFrom4to5 += 1
    } else if number >= -4.34 && number <= -3.32 {
        numbersFrom5to6 += 1
    } else if number >= -3.32 && number <= -2.3 {
        numbersFrom6to7 += 1
    } else if number >= -2.3 && number <= -1.28 {
        numbersFrom7to8 += 1
    } else if number >= -1.28 && number <= -0.26 {
        numbersFrom8to9 += 1
    } else if number >= -0.26 && number <= 0.78 {
        numbersFrom9to10 += 1
    }  else if number >= 0.78 && number <= 1.8 {
        x1 += 1
    }  else if number >= 1.8 && number <= 2.82 {
        x2 += 1
    }  else if number >= 2.82 && number <= 3.84 {
        x3 += 1
    }  else if number >= 3.84 && number <= 4.86 {
        x4 += 1
    }
}
