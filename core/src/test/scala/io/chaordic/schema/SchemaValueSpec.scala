package io.chaordic.schema

import java.time.{LocalDate, LocalDateTime, LocalTime}

import cats.data.Validated.Valid
import org.scalatest.{FlatSpec, Matchers}

class SchemaValueSpec extends FlatSpec with Matchers{

  val dateTime = LocalDateTime.now()
  val date = LocalDate.now()
  val time = LocalTime.now()

  "datetime" should "deal with datetime" in{
    schemaValToDateTime(dateTimeToSchemaVal(dateTime), Nil) should be(Valid(dateTime))
  }

  "date" should "deal with date" in{
    schemaValToDate(dateToSchemaVal(date), Nil) should be(Valid(date))
  }

  "time" should "deal with time" in{
    schemaValToTime(timeToSchemaVal(time), Nil) should be(Valid(time))
  }


  "datetime" should "deal with past datetime" in{
    pastschemaValToDateTime(pastdateTimeToSchemaVal(dateTime.past), Nil) should be(Valid(dateTime.past))
  }

  "date" should "deal with past date" in{
    pastschemaValToDate(pastdateToSchemaVal(date.minusDays(1).past), Nil) should be(Valid(date.minusDays(1).past))
  }

  "time" should "deal with past time" in{
    pastschemaValToTime(pasttimeToSchemaVal(time.past), Nil) should be(Valid(time.past))
  }

  "datetime" should "deal with past" in{
    an [IllegalArgumentException] should be thrownBy dateTime.plusDays(1).past
  }

  "date" should "deal with past" in{
    an [IllegalArgumentException] should be thrownBy date.plusDays(1).past
  }

  "time" should "deal with past" in{
    an [IllegalArgumentException] should be thrownBy time.plusHours(1).past
  }


  "email" should "accept valid formats" in{
    "fname_lname@sub.domain.com".email
    "fname.lname@sub.domain.com".email
    "fname+lname@domain.com".email
    "fna-me+lname@domain.com".email
  }

  "email" should "discard invalid formats" in{
    an [IllegalArgumentException] should be thrownBy "fname.lname@subdomaincom".email
    an [IllegalArgumentException] should be thrownBy "fname+lname@domain.co-m".email
    an [IllegalArgumentException] should be thrownBy "fname+lnamedomain.com".email
    an [IllegalArgumentException] should be thrownBy "@dom.ain.com".email
//    an [IllegalArgumentException] should be thrownBy ".@domain.com".email is this a valid or invalid pattern?
    an [IllegalArgumentException] should be thrownBy "fna-me+ln ame@domain.com".email
    an [IllegalArgumentException] should be thrownBy "fna-me+ln ame@dom ain.com".email
  }



}
