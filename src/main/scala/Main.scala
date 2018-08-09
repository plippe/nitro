package com.github.plippe.nitro

import cats.implicits.{catsStdInstancesForTry => _, _}
import cats.MonadError
import scala.util.Try

object Main {

  case object NoSuchLocationArgument
      extends Throwable("Location argument required")
  case object TypeConstraintLocationArgument
      extends Throwable("Location must be a positive integer")

  def parseInt(str: String): Option[Int] = Try(str.toInt).toOption

  def run[F[_]](grid: Grid[F], args: Array[String])(
      implicit F: MonadError[F, Throwable]): F[(Int, Int)] = {
    for {
      location <- args.headOption
        .fold(F.raiseError[String](NoSuchLocationArgument))(_.pure[F])
      intLocation <- parseInt(location)
        .fold(F.raiseError[Int](TypeConstraintLocationArgument))(_.pure[F])
      uIntLocation <- UInt.fromInt[F](intLocation)
      distance <- grid.distance(uIntLocation)
    } yield (intLocation, distance.value)
  }

  def main(args: Array[String]): Unit = {
    type F[T] = Either[Throwable, T]

    run[F](OddSpiralGrid[F](), args) match {
      case Right((location, distance)) =>
        println(s"From location $location the distance is $distance.")
      case Left(err) => println(s"Error: $err")
    }
  }
}
