package utils

import com.google.common.base
import com.google.common.base.Predicate
import utils.collections.{EnhancedIterable, FasterMutableListWrapper}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.Try

object ImplicitConversions {
  import scala.jdk.CollectionConverters._
  implicit def anyToRunnable[F](f: () => F): Runnable = new Runnable {
    override def run(): Unit = f()
  }

  implicit def wrapInOption[A <: Any](a: A): Option[A] = Option(a)

  implicit def toFasterMutableList[A](mutableList: ListBuffer[A]): FasterMutableListWrapper[A] = new FasterMutableListWrapper[A](mutableList)

  implicit def fromEnhancedIterable[A](enhancedTraversable: EnhancedIterable[A]): IterableOnce[A] =
    enhancedTraversable.iterable

  implicit def toEnhancedIterable[A](list: java.util.List[A]): EnhancedIterable[A] =
    new EnhancedIterable[A](list.asScala)

  implicit def toEnhancedIterable[A](traversable: Iterable[A]): EnhancedIterable[A] =
    new EnhancedIterable[A](traversable)

  implicit def toEnhancedIterable[A](traversable: IterableOnce[A]): EnhancedIterable[A] =
    new EnhancedIterable[A](traversable.iterator.to(Iterable))

  implicit def toDouble(bigDecimal: BigDecimal): Double = bigDecimal.toDouble

  implicit def toLong(bigInt: BigInt): Long = bigInt.toLong

  implicit def toGuavaFunction[From, To](fn: From => To): base.Function[From, To] =
    new base.Function[From, To] {
      override def apply(input: From): To = fn(input)
    }

  implicit def toGuavaPredicate[From](fn: From => Boolean): base.Predicate[From] =
    new Predicate[From] {
      override def apply(input: From): Boolean = fn(input)
    }

  implicit class NotNullOption[T](val t: Try[T]) extends AnyVal {
    def toNotNullOption = t.toOption.flatMap{Option(_)}
  }
}
