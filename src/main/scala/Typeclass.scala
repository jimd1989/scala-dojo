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

trait JsonWriter[A] {
  def write(in: A): String
}

object JsonWriter {
  def write[A](a: A)(implicit w: JsonWriter[A]): String = w.write(a)
  implicit class Ops[A](a: A) {
    def writeJson(implicit jsonWriter: JsonWriter[A]): String = {
      jsonWriter.write(a)
    }
  }
}

final case class Person(name: String, email: String)

object Person {
  implicit val jsonWriterForPerson: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(p: Person): String = {
        val name = p.name
        val email = p.email
        s"""{"name": "$name", "email": "$email"}"""
      }
    }
  // hint: https://www.scala-lang.org/api/current/scala/math/Ordering$.html#fromLessThan[T](cmp:(T,T)=%3EBoolean):scala.math.Ordering[T]
  implicit val sortablePerson: Ordering[Person] =
    new Ordering[Person] {
      def compare(x: Person, y: Person): Int = x.name.compare(y.name)
    }
}

final case class Cat(name: String, food: String)

object Cat {
  implicit val jsonWriterForCat: JsonWriter[Cat] =
    new JsonWriter[Cat] {
      def write(in: Cat): String =
        s"""{"name": "${in.name}", "food": "${in.food}"}"""
    }
}

final case class CatPerson(person: Person, cat: Cat)

object CatPerson {
  implicit val jsonWriterForCatPerson: JsonWriter[CatPerson] =
    new JsonWriter[CatPerson] {
      def write(cp: CatPerson): String =
        s"""{"person":${JsonWriter.write(cp.person)},"cat":${JsonWriter.write(
          cp.cat)}}"""
    }
  implicit class CatPersonOps(cp: CatPerson) {
    def writeJson: String = ???
  }
}
