package io.chaordic

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}

import io.chaordic.schema.SchemaVal.Str
import shapeless.tag
import shapeless.tag.@@

import scala.util.Try


/**
  * Package object containing some common types and core type-class implementations for convenience.
  */
package object schema {
  import FromSchemaVal._

  sealed trait EmailTag
  type Email = String @@ EmailTag

  object Email{
    implicit val emailToSchemaVal = new ToSchemaVal[Email] {
      override def apply(value: Email) = Str(value.toString)
    }
    implicit val schemaValToEmail: FromSchemaVal[Email] = new FromStrSchemaVal[Email] {
      override def fromStr(s: String, path: List[String]) = Try(s.email).toOption.toResult(s"$s is not a valid Email", path)
    }

  }

  private val ePattern = "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$";

  implicit class EmailOps(s: String){
    def email: Email = {
      val p = java.util.regex.Pattern.compile(ePattern)
      val m = p.matcher(s)
      if(m.matches()) {
        tag[EmailTag][String](s)
      }else{
        throw new IllegalArgumentException(s"$s is not a valid e-mail")
      }
    }
  }


  // Should these be implicit parameters?
  def dateTimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSS") // "Z" at the end for UTC, + or -hh:mm for timezone offset
  def dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  def timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS")

  implicit val dateTimeToSchemaVal = new ToSchemaVal[LocalDateTime] {
    override def apply(value: LocalDateTime) = Str(value.format(dateTimeFormat))
  }
  implicit val schemaValToDateTime: FromSchemaVal[LocalDateTime] = new FromStrSchemaVal[LocalDateTime] {
    override def fromStr(s: String, path: List[String]) = {
      Try(LocalDateTime.from(dateTimeFormat.parse(s))).toOption.toResult(s"$s is not a valid LocalDateTime", path)
    }
  }

  implicit val dateToSchemaVal = new ToSchemaVal[LocalDate] {
    override def apply(value: LocalDate) = Str(value.format(dateFormat))
  }
  implicit val schemaValToDate: FromSchemaVal[LocalDate] = new FromStrSchemaVal[LocalDate] {
    override def fromStr(s: String, path: List[String]) = Try(LocalDate.from(dateFormat.parse(s))).toOption.toResult(s"$s is not a valid LocalDate", path)
  }

  implicit val timeToSchemaVal = new ToSchemaVal[LocalTime] {
    override def apply(value: LocalTime) = Str(value.format(timeFormat))
  }
  implicit val schemaValToTime: FromSchemaVal[LocalTime] = new FromStrSchemaVal[LocalTime] {
    override def fromStr(s: String, path: List[String]) = Try(LocalTime.from(timeFormat.parse(s))).toOption.toResult(s"$s is not a valid LocalTime", path)
  }

  sealed trait PastTag
  type PastDateTime = LocalDateTime @@ PastTag
  type PastDate = LocalDate @@ PastTag
  type PastTime = LocalTime @@ PastTag

  implicit class TimeOps(time: LocalTime){
    def past: PastTime = {
      if(time.isBefore(LocalTime.now())){
        tag[PastTag][LocalTime](time)
      }else{
        throw new IllegalArgumentException(s"$time is not in the past")
      }
    }
  }

  implicit class DateOps(time: LocalDate){
    def past: PastDate = {
      if(time.isBefore(LocalDate.now())){
        tag[PastTag][LocalDate](time)
      }else{
        throw new IllegalArgumentException(s"$time is not in the past")
      }
    }
  }

  implicit class DateTimeOps(time: LocalDateTime){
    def past: PastDateTime = {
      if(time.isBefore(LocalDateTime.now())){
        tag[PastTag][LocalDateTime](time)
      }else{
        throw new IllegalArgumentException(s"$time is not in the past")
      }
    }
  }

  implicit val pastdateTimeToSchemaVal = new ToSchemaVal[PastDateTime] {
    override def apply(value: PastDateTime) = Str(value.format(dateTimeFormat))
  }
  implicit val pastschemaValToDateTime: FromSchemaVal[PastDateTime] = new FromStrSchemaVal[PastDateTime] {
    override def fromStr(s: String, path: List[String]) = Try(LocalDateTime.from(dateTimeFormat.parse(s)).past).toOption.toResult(s"$s is not a valid past LocalDateTime", path)
  }

  implicit val pastdateToSchemaVal = new ToSchemaVal[PastDate] {
    override def apply(value: PastDate) = Str(value.format(dateFormat))
  }
  implicit val pastschemaValToDate: FromSchemaVal[PastDate] = new FromStrSchemaVal[PastDate] {
    override def fromStr(s: String, path: List[String]) = Try(LocalDate.from(dateFormat.parse(s)).past).toOption.toResult(s"$s is not a valid past LocalDate", path)
  }

  implicit val pasttimeToSchemaVal = new ToSchemaVal[PastTime] {
    override def apply(value: PastTime) = Str(value.format(timeFormat))
  }
  implicit val pastschemaValToTime: FromSchemaVal[PastTime] = new FromStrSchemaVal[PastTime] {
    override def fromStr(s: String, path: List[String]) = Try(LocalTime.from(timeFormat.parse(s)).past).toOption.toResult(s"$s is not a valid past LocalTime", path)
  }

}
