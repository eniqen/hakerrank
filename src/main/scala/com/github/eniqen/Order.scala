package com.github.eniqen

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
case class Order(arrivalTime: Long, cookingTime: Long)

object Order {
  implicit val cookingTimeOrder: Ordering[Order] =
    Ordering.by[Order, Long](_.cookingTime).reverse

  final implicit class OrderOps(val o: Order) extends AnyVal {
    def prolongate(time: Long): Long = time + o.cookingTime
  }
}
