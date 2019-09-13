package typeclass

sealed trait TrafficLight {
  def next: TrafficLight = ???
}

final case object Red extends TrafficLight {
  override def next: TrafficLight = Green
}
final case object Green extends TrafficLight {
  override def next: TrafficLight = Yellow
}
final case object Yellow extends TrafficLight {
  override def next: TrafficLight = Red
}

sealed trait LinkedList[A] {
  def apply(index: Int): A = ???
}

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
  override def apply(index: Int): A =
    if (index == 0) this.head else this.tail(index - 1)
}

final case class End[A]() extends LinkedList[A] {
  override def apply(index: Int): A = throw new Exception("Out of Boundary")
}

sealed trait CoLinkedList[+A] {
  def apply(index: Int): A = ???
}
final case class CoPair[A](head: A, tail: CoLinkedList[A])
    extends CoLinkedList[A]
final case object CoEnd extends CoLinkedList[Nothing]

// Typeclass signatures go here
trait JsonWriter[A] {
  def write(in: A): String
}

// Typeclass instances go here: the type-specific implementations of a given procedure outlined in the trait
// This will have to be explicitly imported with `import JsonWriterInstances._`
// This is an alternative to
object JsonWriterInstances {
  implicit val personWriter =
    new JsonWriter[Person] {
      def write(p: Person): String = {
        val name = p.name
        val email = p.email
        s"""{"name": "$name", "email": "$email"}"""
      }
    }
}

// Typeclass interface goes here: provide a generic function that will match against specific instances in scope
object JsonWriter {
  def write[A](a: A)(implicit w: JsonWriter[A]): String = w.write(a)
  implicit class Ops[A](a: A) {
    def writeJson(implicit jsonWriter: JsonWriter[A]): String = ???
  }
}

final case class Person(name: String, email: String)

object Person {
  implicit val jsonWriterForPerson: JsonWriter[Person] = ???
  // hint: https://www.scala-lang.org/api/current/scala/math/Ordering$.html#fromLessThan[T](cmp:(T,T)=%3EBoolean):scala.math.Ordering[T]
  //implicit val sortablePerson: Ordering[Person] = ???
}

final case class Cat(name: String, food: String)

object Cat {
  implicit val jsonWriterForCat: JsonWriter[Cat] = ???
}

final case class CatPerson(person: Person, cat: Cat)

object CatPerson {
  implicit val jsonWriterForCatPerson: JsonWriter[CatPerson] = ???
  implicit class CatPersonOps(cp: CatPerson) {
    def writeJson: String = ???
  }
}
