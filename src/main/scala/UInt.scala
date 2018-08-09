package com.github.plippe.nitro

import cats.ApplicativeError

trait UIntError extends Throwable
case object UIntNegativeValueError extends UIntError

case class UInt(value: Int) {
  require(value >= 0, "Unsigned int should be positive")
}

object UInt {
  def fromInt[F[_]](value: Int)(
      implicit F: ApplicativeError[F, Throwable]): F[UInt] = {
    if (value < 0) F.raiseError(UIntNegativeValueError)
    else F.pure(UInt(value))
  }

  def fromIntUnsafe(value: Int) = UInt(value)
}
