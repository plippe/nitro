package com.github.plippe.nitro

import cats.implicits._

class UIntSpec extends org.specs2.mutable.Specification {

  "UInt" >> {
    "fromInt" >> { fromInt() }
    "fromInt fails with negative" >> { fromIntNegative() }
  }

  type F[T] = Either[UIntError, T]

  def fromInt() = {
    UInt.fromInt[F](1) must beRight(UInt(1))
  }

  def fromIntNegative() = {
    UInt.fromInt[F](-1) must beLeft
  }
}
