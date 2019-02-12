package io.chaordic.prop

import enumeratum.{Enum, EnumEntry}
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.{Arbitrary, Prop}
import org.scalatest.prop.Checkers._

sealed trait Greeting extends EnumEntry

object Greeting extends Enum[Greeting] {

  val values = findValues

  case object Hello   extends Greeting
  case object GoodBye extends Greeting
  case object Hi      extends Greeting
  case object Bye     extends Greeting

}

class GeneratorsSpec extends FlatSpec with Matchers{
//  import Generators._
//
//  case class Person(name:String, address: Address, preferredGreeting: Greeting)
//  case class Address(street:String, zip:Int, city: List[City])
//  case class City(townName: String, postCode: Option[String])

 // implicit val greetingArb: Arbitrary[Greeting] = enumArbitrary(Greeting)

//
//  "Generators" should "be able to generate a nested case class" in{
//    check(Prop.forAll((n: Person) => n == n)) //nothing to test really, other than "it compiles and runs"
//  }

}
