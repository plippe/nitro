package com.github.plippe.nitro.math

class MathSpec extends org.specs2.mutable.Specification {

  "math" >> {
    "sq" >> { square() }
  }

  def square() = {
    sq(1) must beEqualTo(1 * 1)
    sq(2) must beEqualTo(2 * 2)
    sq(3) must beEqualTo(3 * 3)
  }
}
