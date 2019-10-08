package com.github.eniqen

import scala.collection.mutable.{PriorityQueue => WaitingQueue}

import scala.annotation.tailrec

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
class Queue {
  import Order._

  def compute(numberOfClients: Int, orders: Orders): Long = orders match {
    case Nil            => 0L
    case o +: Nil       => o.cookingTime
    case seq@ o +: tail =>
      loop(
          o.prolongate(o.arrivalTime)
        , o.cookingTime
        , tail
        , WaitingQueue.empty[Order](Order.cookingTimeOrder)
      ) / numberOfClients

  }

  @tailrec
  private def loop(nextTime: Long
                  , waitingTime: Long
                  , orders: Orders
                  , wq: WaitingQueue[Order]
                  ): Long = orders -> wq.isEmpty match {
    case (Nil, true)  => waitingTime
    case (Nil, false) =>
      val first = wq.dequeue()
      val next  = first.prolongate(nextTime)
      loop(
          next
        , waitingTime + (next - first.arrivalTime)
        , orders
        , wq
      )
    case (seq@h +: t, _) =>
      val last                    = fillBatch(seq)(_.arrivalTime <= nextTime)(wq.enqueue)
      val (head, tail, updatedWq) = wq.headOption.fold((h, t, wq))(fromQueue => (fromQueue, last, wq.tail))
      val next                    = head.prolongate(nextTime)
      loop(
          next
        , waitingTime + (next - head.arrivalTime)
        , tail
        , updatedWq
      )
  }

  private def fillBatch(orders: Orders)
                       (pred: Order => Boolean)
                       (action: Orders => Unit) : Orders = {
    val (addToWq, last) = separateWhile(orders)(pred)
    action(addToWq)
    last
  }

  private def separateWhile(orders: Orders)
                           (pred: Order => Boolean): (Orders, Orders) = {
    @tailrec
    def go(orders: Orders, acc: (Orders, Orders)): (Orders, Orders) = orders match {
      case Nil                  => acc.copy(_1 = acc._1.reverse)
      case h +: tail if pred(h) => go(tail, acc.copy(_1 = h +: acc._1))
      case tail                 => acc.copy(_2 = tail)
    }
    go(orders, Seq.empty -> Seq.empty)
  }
}
