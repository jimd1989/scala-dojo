package monad

import cats.Apply
import cats.data.Validated
import cats.data.Validated.Valid
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import cats.{Functor, Monad}
import cats.data.{EitherT, Reader, Validated, Writer}
import cats.Contravariant
import cats.instances.future._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.writer._
import scala.util.Try
import cats.syntax.traverse._
sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](value: Tree[A])(f: A => B): Tree[B] = value match {
      case Branch(left, right) => branch(map(left)(f), map(right)(f))
      case Leaf(value)         => leaf(f(value))
    }
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

trait Printable[A] { self =>
  def format(value: A): String
}

// (b -> a) -> F a -> F b
object Printable {
  implicit val printableContravariant = new Contravariant[Printable] {
    def contramap[A, B](fa: Printable[A])(func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String = fa.format(func(value))
      }
  }

  def format[A: Printable](value: A): String = {
    implicitly[Printable[A]].format(value)
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    new Printable[Box[A]] {
      def format(b: Box[A]): String =
        printableContravariant.contramap[A, Box[A]](p)(b => b.value).format(b)
    }
}

final case class Box[A](value: A)

object IdMonad {
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))
}

object Writers {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(1)

  def factorial(n: Int): Logged[Int] = {
    for {
      x <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $x").tell
    } yield x
  }
}
// Monad allows sequencing operations that depend on some input. Wrap up one-arg functions.
// If there are a number of operations that depend on an external configuration, they can be chained with a Reader
// to produce one large operation that accepts the config as a parameter and runs the program in the specified order.

// Reader[A, B] is created from a function A => B using Reader.apply constructor.
// case class Cat(name: String, food: String)
// val catName: Reader[Cat, String] = Reader(cat => cat.name)
// catName.run(Cat("Garfield", "Lasagna"))
// --> cats.Id[String] = Garfield

// Readers are useful through the map and flatMap functions, which allow composition.
// Create a set of Readers that operate over the same config and chain them together.
// map extends a computation by passing its result through a function:

// val greetCat: Reader[Cat, String] = catName.map(name => s"Hello ${name}")
// greetCat.run(Cat("Mittens", "food"))
// --> cats.Id[String] = "Hello Mittens"

// flatMap combines Readers that depend on the same input type.
// val feedCat: Reader[Cat, String] = Reader(cat => s"Eat the ${cat.food}")

// for {
//   greet <- greetCat
//   feed  <- feedCat
//  } yield s"$greet. feed."

object Readers {
  case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    Reader(
      db =>
        findUsername(userId)
          .run(db)
          .map(x => checkPassword(x, password))
          .getOrElse(false.pure[DbReader])
          .run(db))
}

// Pass additional state in computation. Atomic state operations threaded together with maps and flatMaps.
// State[S, A] represents functions of type S => (S, A) where S is state type and A is the result.
// It transforms and input state to an output state and computes a result.
// The monad is run by providing an initial state. It has run, runS, and runA, which each return different combinations.
// Each returns an instance of Eval to maintain stack safety.

object States {
  import cats.data.State
  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { state =>
    sym match {
      case "+" => ???
      case "*" => ???
      case _   => ???
    }
  }

  def evalAll(exprs: List[String]): CalcState[Int] = ???
}

object Transformer {

  lazy val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(bot) => ???
      case None      => ???
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = ???

  def tacticalReport(ally1: String, ally2: String): String = ???
}

case class Cat(name: String, birth: Int, food: String)
object SemiNAppli {
  def createCatFromFuture(name: Future[String],
                          birth: Future[Int],
                          food: Future[String]): Future[Cat] = ???

  type AllErrorOr[A] = Validated[List[String], A]
  type FormData = Map[String, String]

  val notBlank = (field: String) =>
    (str: String) =>
      if (str.trim.isEmpty()) Left(List(field + " should not be blank"))
      else Right(str)

  def createCatFromForm(data: FormData): AllErrorOr[Cat] = ???
  def applyCat(data: FormData): AllErrorOr[Cat] = ???
}

object UptimeRobot {
  def getUptime(host: String) = Future(host.length * 60)

  def allUptimes(hosts: List[String]): Future[List[Int]] = ???

  def asyncGetUptimes(uptimes: List[Future[Int]]): Future[List[Int]] = ???
}
