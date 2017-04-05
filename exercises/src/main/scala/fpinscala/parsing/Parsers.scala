package fpinscala.parsing

import fpinscala.testing._

import scala.language.higherKinds

trait Parsers[Parser[+ _]] { // + _ is a type parameter that is itself a type constructor
  self => // so inner classes (e.g. ParserOps) may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String] // so you can write expressions like "abra" | "cadabra"

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def many[A](p: Parser[A]): Parser[List[A]]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  // return the portion of the input string examined by the parser if successful
  def slice[A](p: Parser[A]): Parser[String]

  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled) // { case ((a, b)) => f(a, b) }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many = self.many(p)

    def many1 = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice = self.slice(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))

    def productMapLaw[A, B, C, D](p1: Parser[A], p2: Parser[B])(f: A => C)(g: B => D)(in: Gen[String]): Prop =
      equal(p1.map(f) ** p2.map(g), (p1 ** p2).map { case (a, b) => (f(a), g(b)) })(in)

    def productAssociativityLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      equal(((p1 ** p2) ** p3) map flatten, (p1 ** (p2 ** p3)) map flatten)(in)

    def flatten[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)

    def flatten[A, B, C](t: (A, (B, C))): (A, B, C) = (t._1, t._2._1, t._2._2)

  }

  val numA = char('a').many.map(_.size)

  val numASlice = char('a').many.slice.map(_.size)

  val zeroOrMoreAFollowedByOneOrMoreB =
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}