package io.chaordic.circe

import io.chaordic.schema.{SchemaVal, ToSchemaVal}
import io.circe.{Json}
import io.circe.syntax._

object ToJson {
  import SchemaVal._

  def apply[A : ToSchemaVal](value: A): Json = {
    ToJson(implicitly[ToSchemaVal[A]].apply(value))
  }

  def apply(value: SchemaVal): Json = {
    value match{
      case Obj(kvs) => Json.fromFields(kvs.map(t => (t._1, ToJson(t._2))))
      case SList(ls) => ls.map(ToJson.apply(_ : SchemaVal)).asJson
      case Null => None.asJson
      case Str(s) => s.asJson
      case IntNum(i) => i.asJson
      case LongNum(l) => l.asJson
      case Bool(b) => b.asJson
      case DoubleNum(d) => d.asJson
    }
  }

}
