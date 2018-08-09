package com.github.plippe.nitro

import cats.implicits._

class UIntSpec extends org.specs2.mutable.Specification {

  "UInt" >> {
    "fromInt" >> { fromInt() }
    "fromInt fails with negative" >> { fromIntNegative() }
    "fromIntUnsafe" >> { fromIntUnsafe() }
    "fromIntUnsafe fails with negative" >> { fromIntUnsafeNegative() }
  }

  type F[T] = Either[UIntError, T]

  def fromInt() = {
    UInt.fromInt[F](1) must beRight(UInt(1))
  }

  def fromIntNegative() = {
    UInt.fromInt[F](-1) must beLeft
  }

  def fromIntUnsafe() = {
    UInt.fromIntUnsafe(1) must beEqualTo(UInt(1))
  }

  def fromIntUnsafeNegative() = {
    UInt.fromIntUnsafe(-1) must throwA[IllegalArgumentException]
  }
}
