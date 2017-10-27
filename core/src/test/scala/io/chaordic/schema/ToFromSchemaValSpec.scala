package io.chaordic.schema

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import enumeratum._
import io.chaordic.schema.SchemaVal.{Obj, Str}

import org.scalatest.prop.Checkers._
import org.scalacheck.Prop

sealed trait Greeting extends EnumEntry

object Greeting extends Enum[Greeting] {

  val values = findValues

  case object Hello   extends Greeting
  case object GoodBye extends Greeting
  case object Hi      extends Greeting
  case object Bye     extends Greeting

}

class ToFromSchemaValSpec extends FlatSpec with Matchers{

  import ToSchemaVal._
  import FromSchemaVal._
  implicit val greetingSchema = enumFromSchema(Greeting)

  case class Person(name:String, address: Address, preferredGreeting: Greeting)
  case class Address(street:String, zip:Int, city: List[City])
  case class City(townName: String, postCode: Option[String])


  val person = Person("Tom", Address("Jefferson st", 10000, City("New York", None) :: Nil), Greeting.Hi)

  "ToSchemaVal" should "be symmetric on an arbitrarily nested case class with options, lists and case object enums" in {
    person.toSchemaVal.fromSchemaVal[Person] should be(Valid(person))
  }

  "FromSchemaVal" should "deal with absent options gracefully" in{
    Obj(List(("townName", Str("Zug")))).fromSchemaVal[City] should be(Valid(City("Zug", None)))
  }

  "FromSchemaVal" should "give accurate error reporting" in{
     Obj(List(("name", Str("Tom")))).fromSchemaVal[Person] should be(
        Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError,List("address")),
          List(ValidationError(NullOrMissingFieldError,List("preferredGreeting"))))))
  }

  "FromSchemaVal" should "give accurate error reporting for nested values" in{
    Obj(List(("name", Str("Tom")), ("address",
      Obj(List(("street", Str("JeffersonStreet")))))
      )).fromSchemaVal[Person] should be(
      Invalid(NonEmptyList(
        ValidationError(NullOrMissingFieldError,List("address", "zip")),
        List(ValidationError(NullOrMissingFieldError,List("address", "city")),
          ValidationError(NullOrMissingFieldError,List("preferredGreeting"))))))

  }


  "BigDecimal" should "be symmetric" in{
    val from = implicitly[FromSchemaVal[BigDecimal]]
    val to = implicitly[ToSchemaVal[BigDecimal]]

    check(Prop.forAll((n: BigDecimal) => from(to(n), Nil) == Valid(n)))
  }

  "Int" should "be symmetric" in{
    val from = implicitly[FromSchemaVal[Int]]
    val to = implicitly[ToSchemaVal[Int]]

    check(Prop.forAll((n: Int) => from(to(n), Nil) == Valid(n)))
  }

  "Long" should "be symmetric" in{
    val from = implicitly[FromSchemaVal[Long]]
    val to = implicitly[ToSchemaVal[Long]]

    check(Prop.forAll((n: Long) => from(to(n), Nil) == Valid(n)))
  }

  "Double" should "be symmetric" in{
    val from = implicitly[FromSchemaVal[Double]]
    val to = implicitly[ToSchemaVal[Double]]

    check(Prop.forAll((n: Double) => from(to(n), Nil) == Valid(n)))
  }


}