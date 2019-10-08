package com.github.eniqen

import java.io.InputStream

import scala.io.Source

object OrderProcessor {

  /**
    * @param in An InputStream, which contains the following input:
    *           A line containing a single number: The number of guests N,
    *           Followed by N lines containing two numbers Ti and Li separated by space.
    *           There may be a trailing newline.
    *           Ti ist the ordering time for Ni, Li is the time it takes to bake Ni's pizza.
    *           0 <= N  <= 100000
    *           0 <= Ti <= 1000000000
    *           1 <= Li <= 1000000000
    * @return   A Right containing the integer part of the average waiting time if the input is valid.
    *           A Left containing a syntax error description otherwise.
    */
  def process(in: InputStream): Either[Seq[Error], Long] = {
    import ValidationUtil._

    val lines = Source.fromInputStream(in).getLines().zipWithIndex.toList

    if (lines.headOption.flatMap(_ => lines.tail.headOption).isEmpty) Right(0L) else {
      for {
        n      <- validateN(lines.head._1).leftMap(Seq(_))
        orders <- validateList(lines.tail)
      } yield new Queue().compute(n, orders)
    }
  }
}
