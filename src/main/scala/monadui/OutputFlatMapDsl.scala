package monadui

import scala.collection.immutable.HashMap

/**
 * A traditional monadic DSL, suitable for use in for comprehensions.
 */
object OutputFlatMapDsl {
  implicit class RichOutput[T](val output: Output[T]) extends AnyVal {
    def flatMap[U](f: T => Output[U]): Output[U] = {
      output.value match {
        case Some(v) => f(v) match {
          case Output(s @ Some(_), written1) => output.withValue(s, written1)
          case Output(None, written1) => output.withValue(None, written1)
        }
        case None => this.asInstanceOf[Output[U]]
      }
    }
    def map[U](f: T => U): Output[U] = {
      if (output.value.isDefined)
        output.withValue(Some(f(output.value.get)), HashMap.empty)
      else this.asInstanceOf[Output[U]]
    }
    def filter(f: T => Boolean): Output[T] = {
      if (output.value.exists(f)) output
      else output.withValue(None, HashMap.empty)
    }
    def withFilter(f: T => Boolean): Output[T] = filter(f)

    def ap2[U](o2: Output[U]): Output[(T, U)] = {
      val w = Output.mergeMultiMap(output.written, o2.written)
      (output, o2) match {
        case (Output(Some(v1), _), Output(Some(v2), _)) ⇒
          new Output(Some((v1, v2)), w)
        case _ ⇒
          new Output(None, w)
      }
    }
  }

  object RichOutput {
    def ap2[T1, T2](o1: Output[T1], o2: Output[T2]): Output[(T1,T2)] = {
      val w = Output.mergeMultiMap(o1.written, o2.written)
      (o1, o2) match {
        case (Output(Some(v1), _), Output(Some(v2), _)) ⇒
          new Output(Some((v1, v2)), w)
        case _ ⇒
          new Output(None, w)
      }
    }
  }
}
