package io.chaordic.schema


sealed trait SchemaVal

sealed trait Primitive extends SchemaVal
sealed trait Complex extends SchemaVal

object SchemaVal {
  case class Str(value: String) extends Primitive
  case class IntNum(value: Int) extends Primitive
  case class LongNum(value: Long) extends Primitive
  case class DoubleNum(value: Double) extends Primitive
  case class Bool(value: Boolean) extends Primitive
  case class Obj(value: List[(String, SchemaVal)]) extends Complex{
    def get(key: String): Option[SchemaVal] = {
      value.find(_._1 == key).map(_._2)
    }
  }

  case class SList(value: List[SchemaVal]) extends Complex
  case object Null extends Primitive

  val Empty = Obj(Nil)
}



