package com.github.plippe.nitro

import cats.implicits._

class GridSpec extends org.specs2.mutable.Specification {

  "OddSpiralGrid" >> {
    "maxCornerValue" >> { maxCornerValue() }
    "distanceFromCorner" >> { distanceFromCorner() }
    "shortestDistanceToCorner on corner" >> {
      shortestDistanceToCornerOnCorner()
    }
    "shortestDistanceToCorner in center" >> {
      shortestDistanceToCornerInCenter()
    }
    "shortestDistanceToCorner around" >> { shortestDistanceToCornerAround() }
    "distance" >> { distance() }
    "distance not on grid" >> { distanceNotOnGrid() }
  }

  type F[T] = Either[GridError, T]

  def maxCornerValue() = {
    val grid = OddSpiralGrid()

    grid.maxCornerValue(1) must beEqualTo(1 * 1)
    grid.maxCornerValue(3) must beEqualTo(3 * 3)
    grid.maxCornerValue(5) must beEqualTo(5 * 5)
  }

  def distanceFromCorner() = {
    val grid = OddSpiralGrid()

    grid.distanceFromCorner(1) must beEqualTo(0)
    grid.distanceFromCorner(3) must beEqualTo(2)
    grid.distanceFromCorner(5) must beEqualTo(4)
  }

  def shortestDistanceToCornerOnCorner() = {
    val grid = OddSpiralGrid()
    grid.shortestDistanceToCorner(7, 31) must beEqualTo(0)
    grid.shortestDistanceToCorner(7, 37) must beEqualTo(0)
    grid.shortestDistanceToCorner(7, 43) must beEqualTo(0)
    grid.shortestDistanceToCorner(7, 49) must beEqualTo(0)
  }

  def shortestDistanceToCornerInCenter() = {
    val grid = OddSpiralGrid()
    grid.shortestDistanceToCorner(7, 28) must beEqualTo(7 / 2)
    grid.shortestDistanceToCorner(7, 34) must beEqualTo(7 / 2)
    grid.shortestDistanceToCorner(7, 40) must beEqualTo(7 / 2)
    grid.shortestDistanceToCorner(7, 46) must beEqualTo(7 / 2)
  }

  def shortestDistanceToCornerAround() = {
    val grid = OddSpiralGrid()
    grid.shortestDistanceToCorner(7, 30) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 32) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 36) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 38) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 42) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 44) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 48) must beEqualTo(1)
    grid.shortestDistanceToCorner(7, 26) must beEqualTo(1)
  }

  def distance() = {
    val grid = OddSpiralGrid()
    println("A")
    grid.distance(UInt(1)) must beRight(UInt(0))
    println("B")
    grid.distance(UInt(12)) must beRight(UInt(3))
    println("C")
    grid.distance(UInt(23)) must beRight(UInt(2))
    println("D")
    grid.distance(UInt(1024)) must beRight(UInt(31))
    println("E")
    grid.distance(UInt(368078)) must beRight
  }

  def distanceNotOnGrid() = {
    val grid = new OddSpiralGrid(UInt(7))
    grid.distance(UInt(0)) must beLeft(ValueNotOnGrid)
    grid.distance(UInt(50)) must beLeft(ValueNotOnGrid)
  }
}
