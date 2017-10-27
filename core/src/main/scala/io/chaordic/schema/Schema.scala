package io.chaordic.schema


sealed trait SchemaVal

object SchemaVal {
  case class Str(value: String) extends SchemaVal
  case class IntNum(value: Int) extends SchemaVal
  case class LongNum(value: Long) extends SchemaVal
  case class DoubleNum(value: Double) extends SchemaVal
  case class Bool(value: Boolean) extends SchemaVal
  case class Obj(value: List[(String, SchemaVal)]) extends SchemaVal{
    def get(key: String): Option[SchemaVal] = {
      value.find(_._1 == key).map(_._2)
    }
  }

  case class SList(value: List[SchemaVal]) extends SchemaVal
  case object Null extends SchemaVal

  val Empty = Obj(Nil)
}



