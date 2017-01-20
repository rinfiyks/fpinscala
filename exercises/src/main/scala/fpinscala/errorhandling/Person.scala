package fpinscala.errorhandling

sealed class Name(val value: String)
sealed class Age(val value: Int)

case class Person(name: Name, age: Age) {

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(): Either[String, Person] =
    mkName(name.value).map2(mkAge(age.value))(Person(_, _))

}

object PersonTester {
  def main(args: Array[String]): Unit = {
    println(Person(new Name(""), new Age(-123)).mkPerson())
    // E 4.8 - new Either class where Left is a seq of errors?
  }
}
