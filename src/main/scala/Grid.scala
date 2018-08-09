package com.github.plippe.nitro

import cats.implicits._
import cats.ApplicativeError

trait GridError extends Throwable
case object ValueNotOnGrid extends GridError

trait Grid[F[_]] {
  def distance(value: UInt): F[UInt]
}

class OddSpiralGrid[F[_]](n: UInt)(implicit F: ApplicativeError[F, Throwable])
    extends Grid[F] {

  def maxCornerValue(squareIndex: Int): Int =
    math.sq(squareIndex.toDouble).toInt

  def distanceFromCorner(squareIndex: Int): Int = squareIndex - 1

  def shortestDistanceToCorner(squareIndex: Int, value: Int): Int = {
    val cornerValue = maxCornerValue(squareIndex)
    val edgeLength = squareIndex - 1

    0.to(4)
      .map(i => cornerValue - edgeLength * i)
      .map(c => scala.math.abs(c - value))
      .min
  }

  def distance(value: UInt): F[UInt] = {
    Range(1, n.value, 2)
      .find { n =>
        value.value >= maxCornerValue(n - 2) &&
        value.value <= maxCornerValue(n)
      }
      .fold(F.raiseError[Int](ValueNotOnGrid))(F.pure)
      .map { n =>
        val d = distanceFromCorner(n) - shortestDistanceToCorner(n, value.value)
        UInt.fromIntUnsafe(d)
      }
  }
}

object OddSpiralGrid {
  val maxSize = Int.MaxValue

  def apply[F[_]]()(implicit F: ApplicativeError[F, Throwable]) = {
    val n = UInt.fromIntUnsafe(maxSize)
    new OddSpiralGrid[F](n)
  }
}
