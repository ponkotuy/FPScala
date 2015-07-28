package chap4

/** Either[E, A]をEither[Seq[E], A]のような型Validation[E, A]とする */
object Exe8 {
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if(name == "" || name == null) Left("Name is empty.") else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if(age < 0) Left("Age is out of range.") else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person)
  def mkPerson_(name: String, age: Int): Validation[String, Person] = mkName(name).map2_(mkAge(age))(Person)

  assert(mkPerson("", 0) == mkPerson_("", 0).toEither)
  assert(mkPerson("a", -1) == mkPerson_("a", -1).toEither)
  assert(mkPerson_("", -1) == Validation.Failure(List("Name is empty.", "Age is out of range.")))
}
