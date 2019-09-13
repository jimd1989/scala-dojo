package classandobject

// * Companion object
class Person(val firstName: String, val lastName: String) {}

object Person {
  def apply(name: String): Person = {
    val fullName = name.split(" ")
    if (fullName.length == 2) new Person(fullName(0), fullName(1))
    else new Person("", "")
  }
}

// * Pattern Matching
case class Cat(color: String, food: String)

object ChipShop {
  def willServe(c: Cat): Boolean = (c.food == "Chips")
}
