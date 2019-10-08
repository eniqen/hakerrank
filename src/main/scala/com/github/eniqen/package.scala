package com.github

package object eniqen {

  type Orders = Seq[Order]

  //some copy from cats core
  final implicit class EitherOps[A, B](private val eab: Either[A, B])
    extends AnyVal {
    def leftMap[C](f: A => C): Either[C, B] = eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => leftCast(r)
    }
    private def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
      right.asInstanceOf[Either[C, B]]
  }

  final implicit class EitherIdOps[A](private val obj: A) extends AnyVal {
    /** Wrap a value in `Left`. */
    def asLeft[B]: Either[A, B] = Left(obj)

    /** Wrap a value in `Right`. */
    def asRight[B]: Either[B, A] = Right(obj)
  }

  sealed trait Error extends Product with Serializable {
    def message: String
  }

  object Error {
    case object InvalidRangeValue extends Error {
      override def message: String = "N must be bigger than 0 and less than 100000"
    }
    case class LiError(index: Int) extends Error {
      override def message: String = s"Index: $index, Li value must be >= 1 and <= 1000000000"
    }
    case class TiError(index: Int) extends Error {
      override def message: String = s"Index: $index, Ti value must be >= than 0 and <= 1000000000"
    }
    case object InvalidLength extends Error {
      override def message: String = "Length must be >= 2"
    }
  }
}
