package io.chaordic.circe

import cats.data.Validated.Valid
import enumeratum.{Enum, EnumEntry}
import io.chaordic.prop.Generators._
import io.chaordic.schema.{FromSchemaVal, ToSchemaVal}
import org.scalacheck.{Arbitrary, Prop}
import org.scalatest.prop.Checkers.check
import org.scalatest.{FlatSpec, Matchers}

class ToFromSpec extends FlatSpec with Matchers {

  sealed trait Greeting extends EnumEntry

  object Greeting extends Enum[Greeting] {

    val values = findValues

    case object Hello   extends Greeting
    case object GoodBye extends Greeting
    case object Hi      extends Greeting
    case object Bye     extends Greeting

  }

  import ToSchemaVal._
  import FromSchemaVal._
  implicit val greetingSchema = enumFromSchema(Greeting)
  implicit val greetingArb: Arbitrary[Greeting] = enumArbitrary(Greeting)

  case class Person(name:String, address: Address, preferredGreeting: Greeting)
  case class Address(street:String, zip:Int, city: List[City])
  case class City(townName: String, postCode: Option[String])

  val person = Person("Tom", Address("Jefferson st", 10000, City("New York", None) :: Nil), Greeting.Hi)

  val personJson = """{"name":"Tom","address":{"street":"Jefferson st","zip":10000,"city":[{"townName":"New York","postCode":null}]},"preferredGreeting":"Hi"}"""

  "FromJson" should "parse the expected result" in{
    FromJson[Person](personJson) should be(Valid(person))
  }

  "ToJson and FromJSON" should "should be symmetric" in{
    check(Prop.forAll((p: Person) => FromJson[Person](ToJson(p)) == Valid(p)))
  }

  "BigDecimal" should "be symmetric" in{
    check(Prop.forAll((n: BigDecimal) => FromJson[BigDecimal](ToJson(n)) == Valid(n)))
  }

  "Int" should "be symmetric" in{
    check(Prop.forAll((n: Int) => FromJson[Int](ToJson(n)) == Valid(n)))
  }

  "Long" should "be symmetric" in{
    check(Prop.forAll((n: Long) => FromJson[Long](ToJson(n)) == Valid(n)))
  }

  "Double" should "be symmetric" in{
    check(Prop.forAll((n: Double) => FromJson[Double](ToJson(n)) == Valid(n)))
  }

  "BigDecimal" should "be symmetric in schema format" in{
    val to = implicitly[ToSchemaVal[BigDecimal]]
    check(Prop.forAll((n: BigDecimal) => FromJson[BigDecimal](ToJson(to(n))) == Valid(n)))
  }

  "Int" should "be symmetric schema format" in{
    val to = implicitly[ToSchemaVal[Int]]
    check(Prop.forAll((n: Int) => FromJson[Int](ToJson(to(n))) == Valid(n)))
  }

  "Long" should "be symmetric schema format" in{
    val to = implicitly[ToSchemaVal[Long]]
    check(Prop.forAll((n: Long) => FromJson[Long](ToJson(to(n))) == Valid(n)))
  }

  "Double" should "be symmetric schema format" in{
    val to = implicitly[ToSchemaVal[Double]]
    check(Prop.forAll((n: Double) => FromJson[Double](ToJson(to(n))) == Valid(n)))
  }

}
