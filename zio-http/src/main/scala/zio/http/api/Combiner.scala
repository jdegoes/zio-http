package zio.http.api

import zio.stacktracer.TracingImplicits.disableAutoTrace // scalafix:ok;

/**
 * A combiner is a type class responsible for combining invariant type
 * parameters. It is used to compose the parameters of the
 * [[zio.http.api.HttpCodec]] data type.
 */
sealed trait Combiner[L, R] {
  type Out

  def combine(l: L, r: R): Out

  def separate(out: Out): (L, R)
}

object Combiner extends CombinerLowPriority1 {
  type WithOut[L, R, Out0] = Combiner[L, R] { type Out = Out0 }

  implicit def leftUnit[A]: Combiner.WithOut[Unit, A, A] =
    new Combiner[Unit, A] {
      type Out = A

      def combine(l: Unit, r: A): A = r

      def separate(out: A): (Unit, A) = ((), out)
    }
}

trait CombinerLowPriority1 extends CombinerLowPriority2 {
  implicit def rightUnit[A]: Combiner.WithOut[A, Unit, A] =
    new Combiner[A, Unit] {
      type Out = A

      def combine(l: A, r: Unit): A = l

      def separate(out: A): (A, Unit) = (out, ())
    }
}

trait CombinerLowPriority2 extends CombinerLowPriority3 {
  // (A, B) + C -> (A, B, C)
  implicit def combine2[A, B, C]: Combiner.WithOut[(A, B), C, (A, B, C)] =
    new Combiner[(A, B), C] {
      type Out = (A, B, C)

      def combine(l: (A, B), r: C): (A, B, C) = (l._1, l._2, r)

      def separate(out: (A, B, C)): ((A, B), C) =
        ((out._1, out._2), out._3)
    }
}

trait CombinerLowPriority3 extends CombinerLowPriority4 {

  // (A, B, C) + D -> (A, B, C, D)
  implicit def combine3[A, B, C, D]: Combiner.WithOut[(A, B, C), D, (A, B, C, D)] =
    new Combiner[(A, B, C), D] {
      type Out = (A, B, C, D)

      def combine(l: (A, B, C), r: D): (A, B, C, D) = (l._1, l._2, l._3, r)

      def separate(out: (A, B, C, D)): ((A, B, C), D) =
        ((out._1, out._2, out._3), out._4)
    }

  // (A, B, C, D) + E -> (A, B, C, D, E)
  implicit def combine4[A, B, C, D, E]: Combiner.WithOut[(A, B, C, D), E, (A, B, C, D, E)] =
    new Combiner[(A, B, C, D), E] {
      type Out = (A, B, C, D, E)

      def combine(l: (A, B, C, D), r: E): (A, B, C, D, E) = (l._1, l._2, l._3, l._4, r)

      def separate(out: (A, B, C, D, E)): ((A, B, C, D), E) =
        ((out._1, out._2, out._3, out._4), out._5)
    }

  // (A, B, C, D, E) + F -> (A, B, C, D, E, F)
  implicit def combine5[A, B, C, D, E, F]: Combiner.WithOut[(A, B, C, D, E), F, (A, B, C, D, E, F)] =
    new Combiner[(A, B, C, D, E), F] {
      type Out = (A, B, C, D, E, F)

      def combine(l: (A, B, C, D, E), r: F): (A, B, C, D, E, F) = (l._1, l._2, l._3, l._4, l._5, r)

      def separate(out: (A, B, C, D, E, F)): ((A, B, C, D, E), F) =
        ((out._1, out._2, out._3, out._4, out._5), out._6)
    }

}

trait CombinerLowPriority4 {

  implicit def combine[A, B]: Combiner.WithOut[A, B, (A, B)] =
    new Combiner[A, B] {
      type Out = (A, B)

      def combine(l: A, r: B): (A, B) = (l, r)

      def separate(out: (A, B)): (A, B) = (out._1, out._2)
    }
}


sealed trait Alternator[L, R] {
  type Out 

  def left(l: L): Option[Out]
  def right(r: R): Out 

  def unleft(out: Out): Option[L]
  def unright(out: Out): Option[R]
}
object Alternator extends AlternatorLowPriority1 {
  type WithOut[L, R, O] = Alternator[L, R] { type Out = O }

  implicit def leftUnit[A]: Alternator.WithOut[Unit, A, A] =
    new Alternator[Unit, A] {
      type Out = A

      def left(l: Unit): Option[Out] = None

      def right(r: A): Out = r

      def unleft(out: Out): Option[Unit] = None

      def unright(out: Out): Option[A] = Some(out)
    }

}

trait AlternatorLowPriority1 extends AlternatorLowPriority2 {
  implicit def merge[A]: Alternator.WithOut[A, A, A] =
    new Alternator[A, A] {
      type Out = A

      def left(l: A): Option[Out] = Some(l)

      def right(r: A): Out = r

      def unleft(out: Out): Option[A] = Some(out)

      def unright(out: Out): Option[A] = Some(out)
    }
}
trait AlternatorLowPriority2 {
  implicit def leftRight[A, B]: Alternator.WithOut[A, B, Either[A, B]] =
    new Alternator[A, B] {
      type Out = Either[A, B]

      def left(l: A): Option[Out] = Some(Left(l))

      def right(r: B): Out = Right(r)

      def unleft(out: Out): Option[A] = out.swap.toOption

      def unright(out: Out): Option[B] = out.toOption
    }
}