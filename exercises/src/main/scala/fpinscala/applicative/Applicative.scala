package fpinscala
package applicative

import java.text.SimpleDateFormat
import java.util.Date

import monads.Functor
import state._
import StateUtil._
import monoids._

import language.higherKinds
import language.implicitConversions
import scala.util.control.NonFatal

trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply[B, C](map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  //  derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply[A, B](unit(f))(fa)

  def mapViaMap2Unit[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit())((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply[A, B => C => D](unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply[A, B => C => D => E](unit(f.curried))(fa))(fb))(fc))(fd)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ea match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (va, vb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(eh1, et1), Failure(eh2, et2)) => Failure(eh1, (et1 :+ eh2) ++ et2)
          case (_, Failure(eh, et)) => Failure(eh, et)
          case (Failure(eh, et), _) => Failure(eh, et)
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, bs) => G.map2(f(a), bs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

object ApplicativeStreamTester extends App {
  val s1: Stream[Int] = Stream.from(0)
  val s2: Stream[Int] = Stream.from(1, 3)
  val s3: Stream[Int] = Stream.from(2, 3)
  val result = Applicative.streamApplicative.sequence(List(s1, s2, s3))
  println(result.take(5).toList)
}

object ApplicativeOptionTester extends App {

  optionApplicativeExample()
  optionMonadExample()

  def AO: Applicative[Option] = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = fab match {
      case Some(f) => fa map f
      case None => None
    }
  }

  def MO: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  private def optionApplicativeExample() = {
    val depts: Map[String, String] = Map("Alice" -> "CS", "Bob" -> "Maths")
    val salaries: Map[String, Int] = Map("Alice" -> 40000, "Bob" -> 35000)
    val employee: String = "Alice"

    val o: Option[String] = AO.map2(depts.get(employee), salaries.get(employee)) {
      (dept, salary) => s"employee in $dept makes $salary per year"
    }

    println(o)
  }

  private def optionMonadExample() = {
    val idsByName: Map[String, Int] = Map("Alice" -> 1, "Bob" -> 2)
    val depts: Map[Int, String] = Map(1 -> "CS", 2 -> "Maths")
    val salaries: Map[Int, Int] = Map(1 -> 40000, 2 -> 35000)
    val employee = "Bob"

    val o: Option[String] = MO.flatMap(idsByName.get(employee))(id =>
      AO.map2(depts.get(id), salaries.get(id)) {
        (dept, salary) => s"$employee in $dept makes $salary per year"
      })

    println(o)
  }

}

object ApplicativeValidationTester extends App {

  case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validateName(name: String): Validation[String, String] = {
    if (name == null || name.isEmpty) Failure("Name cannot be empty")
    else Success(name)
  }

  def validateBirthDate(birthDate: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
    } catch {
      case NonFatal(_) => Failure("Birth date must be in the format yyyy-MM-dd")
    }

  def validatePhoneNumber(phoneNumber: String): Validation[String, String] = {
    if (phoneNumber != null && phoneNumber.matches("[0-9]{10,12}")) Success(phoneNumber)
    else Failure("Phone number must be 10 to 12 digits")
  }

  def validWebForm(name: String, birthDate: String, phoneNumber: String): Validation[String, WebForm] =
    Applicative.validationApplicative.map3(
      validateName(name),
      validateBirthDate(birthDate),
      validatePhoneNumber(phoneNumber)
    )(WebForm)

  println(validWebForm("Alice", "1990-01-01", "07123456789"))
  println(validWebForm("Bob", "1990/01/01", "07123456789"))
  println(validWebForm("Carol", "07123456789", "1990-01-01"))
  println(validWebForm("", "1990-01-01", "07123456789"))
  println(validWebForm(null, null, null))
}

