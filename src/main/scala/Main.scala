package com.github.plippe.nitro

object Main extends App {

    def distance(l: Int) = {
        Range(1, Int.MaxValue, 2)
            .find(n => math.pow(n, 2) >= l)
            .map { n =>
                val pow = math.pow(n, 2).toInt

                val stepsFromCorner = n - 1
                val stepsToCorner = 0.to(4)
                    .map(i => pow - (n-1) * i)
                    .map(c => math.abs(c - l))
                    .min

                stepsFromCorner - stepsToCorner
            }
    }

    assert(distance(1) == Some(0))
    assert(distance(12) == Some(3))
    assert(distance(23) == Some(2))
    assert(distance(1024) == Some(31))

    println("Hello World")
}
