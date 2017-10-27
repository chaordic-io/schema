package io.chaordic.circe

import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated}
import io.chaordic.schema.{FormatError, FromSchemaVal, SchemaVal, ValidationError}
import io.circe._
import io.chaordic.schema.SchemaVal._
import io.circe.parser._

object FromJson {

  def apply[A : FromSchemaVal](jsonString: String): Validated[NonEmptyList[ValidationError], A] = {
    parse(jsonString).fold(fa =>
      Invalid(NonEmptyList.one(ValidationError(FormatError(s"$jsonString is not valid JSON"), Nil))),
      json => FromJson.apply(json))
  }

  def apply[A : FromSchemaVal](json: Json): Validated[NonEmptyList[ValidationError], A] = {
    implicitly[FromSchemaVal[A]].apply(toSchemaVal(json), Nil)
  }

  def toSchemaVal(json: Json): SchemaVal = {
    (json.asObject.map(obj => {
       Obj(obj.toList.map(f => (f._1, toSchemaVal(f._2))))
    }) orElse json.asArray.map(arr => {
      SList(arr.toList.map(a => toSchemaVal(a)))
    }) orElse json.asString.map(Str) orElse json.asBoolean.map(Bool) orElse
    json.asNumber.map(j => {
      if(j.toString.indexOf(".") < 0){
        LongNum(j.truncateToLong)
      }else{
        DoubleNum(j.toDouble)
      }
    })).getOrElse(Null)
  }

}