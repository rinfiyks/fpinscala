package fpinscala.parsing

import scala.language.higherKinds

trait Parsers[Parser[+ _]] { // + _ is a type parameter that is itself a type constructor
  self => // so inner classes (e.g. ParserOps) may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String] // so you can write expressions like "abra" | "cadabra"

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def count[A](c: Char): Parser[Int]

  def countExists[A](c: Char): Parser[Int]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def combine[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def +[B >: A](p2: Parser[B]): Parser[B] = self.combine(p, p2)

    def combine[B >: A](p2: => Parser[B]): Parser[B] = self.combine(p, p2)
  }

  object Laws {
  }

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