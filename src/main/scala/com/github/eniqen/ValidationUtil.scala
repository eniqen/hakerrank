package com.github.eniqen

import com.github.eniqen.Error._

import scala.util.Try

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object ValidationUtil {
  def validateN(n: String): Either[Error, Int] = {
    val v = Try(n.toInt).filter(v => v >= 0 && v <= 100000)
    Either.cond(
      v.isSuccess,
      v.get,
      InvalidRangeValue
    )
  }

  def validateTi(ti: String)(index: Int): Either[Error, Long] = {
    val v = Try(ti.toLong).filter(v => v >= 0 && v <= 1000000000L)
    Either.cond(
      v.isSuccess,
      v.get,
      TiError(index)
    )
  }

  def validateLi(li: String)(index: Int): Either[Error, Long] = {
    val v = Try(li.toLong).filter(v => v >= 1 && v <= 1000000000L)
    Either.cond(
      v.isSuccess,
      v.get,
      LiError(index)
    )
  }

  def validateList(
      i: List[(String, Int)]): Either[Seq[Error], Seq[Order]] = {
    i.foldLeft(Seq.empty[Order].asRight[Seq[Error]]) {
        case (acc, v) =>
          acc -> validateOrder(v._1, v._2) match {
            case (Right(orders), Right(order)) =>
              (order +: orders).asRight[Seq[Error]]
            case (Left(lErr), Left(rErr)) => (rErr +: lErr).asLeft[Seq[Order]]
            case (Left(err), _)           => err.asLeft[Seq[Order]]
            case (_, Left(err))           => Seq(err).asLeft[Seq[Order]]
          }
      }
      .map(_.sortBy(_.arrivalTime)).leftMap(_.reverse)
  }

  def validateOrder(line: String, index: Int): Either[Error, Order] = {
    for {
      res <- checkSize(line)
      ti  <- validateTi(res._1)(index)
      li  <- validateLi(res._2)(index)
    } yield Order(ti, li)
  }

  def checkSize(line: String): Either[Error, (String, String)] = {
    val arr = line.trim.split(" ")
    Either.cond(
      arr.size == 2,
      arr(0) -> arr(1),
      InvalidLength
    )
  }
}
