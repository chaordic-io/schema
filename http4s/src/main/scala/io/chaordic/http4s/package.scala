package io.chaordic

import cats.data.{NonEmptyList, Validated}
import cats.effect.IO
import io.chaordic.schema.SchemaVal.Obj
import io.chaordic.schema.{FromSchemaVal, SchemaVal, ValidationError}
import org.http4s.Request

package object http4s {


  implicit class RequestOps(request: Request[IO]){
    def as[A : FromSchemaVal]: Validated[NonEmptyList[ValidationError],A] = {
      fromMultiParams[A](request.multiParams)
    }
  }

  def fromMultiParams[A : FromSchemaVal](params: Map[String, Seq[String]]): Validated[NonEmptyList[ValidationError],A] = {
    val fromVal = implicitly[FromSchemaVal[A]]
    fromVal(Obj(params.map(tuple => {
      val (key, value) = tuple
      value.toList match{
        case Nil => (key, SchemaVal.Null)
        case s :: Nil => (key, SchemaVal.Str(s)) // this may strictly not be good, but we have no idea from a Map[String,Seq[String]] if this is a correct assumption
        case list => (key, SchemaVal.SList(list.map(SchemaVal.Str)))
      }
    }).toList), Nil)
  }


}
