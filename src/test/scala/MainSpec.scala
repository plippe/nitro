package com.github.plippe.nitro

class MainSpec extends org.specs2.mutable.Specification {

  "Main" >> {
    "hello test" >> { helloTest() }
  }

  def helloTest() = {
    0 must beEqualTo(0)
  }
}
