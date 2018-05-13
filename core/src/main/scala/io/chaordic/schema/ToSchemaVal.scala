package io.chaordic.schema

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import enumeratum.EnumEntry
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType

trait ToSchemaVal[A]{
  def apply(value: A): SchemaVal
}

object ToSchemaVal{

  implicit val validationMessageToSchema: ToSchemaVal[ValidationMessage]= new ToSchemaVal[ValidationMessage]{
    def apply(s: ValidationMessage) = s match{
      case NullOrMissingFieldError => SchemaVal.Obj(List(("errorType", SchemaVal.Str("NullOrMissingField"))))
      case FormatError(msg) => SchemaVal.Obj(List(("errorType", SchemaVal.Str("FormatError")), ("msg", SchemaVal.Str(msg))))
    }
  }

  implicit def enumEntryToSchema[A <: EnumEntry] = new ToSchemaVal[A] {
    def apply(a: A) = SchemaVal.Str(a.entryName)
  }

  implicit val stringToSchema: ToSchemaVal[String]= new ToSchemaVal[String]{
    def apply(s: String) = SchemaVal.Str(s)
  }

  implicit val uuidToSchema: ToSchemaVal[UUID]= new ToSchemaVal[UUID]{
    def apply(s: UUID) = SchemaVal.Str(s.toString)
  }

  implicit val bigDecimalToSchema: ToSchemaVal[BigDecimal]= new ToSchemaVal[BigDecimal]{
    def apply(s: BigDecimal) = SchemaVal.Str(s.toString())
  }

  implicit val intToSchema: ToSchemaVal[Int] = new ToSchemaVal[Int]{
    def apply(s: Int) = SchemaVal.IntNum(s)
  }
  implicit val doubleToSchema: ToSchemaVal[Double] = new ToSchemaVal[Double]{
    def apply(s: Double) = SchemaVal.DoubleNum(s)
  }

  implicit val longToSchema: ToSchemaVal[Long]= new ToSchemaVal[Long]{
    def apply(s: Long) = SchemaVal.LongNum(s)
  }

  implicit val booleanToSchema: ToSchemaVal[Boolean] = new ToSchemaVal[Boolean]{
    def apply(s: Boolean) = SchemaVal.Bool(s)
  }
  

  implicit def optionToSchema[A : ToSchemaVal]: ToSchemaVal[Option[A]] = new ToSchemaVal[Option[A]]{
    def apply(s: Option[A]) = {
      val toSchema = implicitly[ToSchemaVal[A]]
      s.map(toSchema.apply _).getOrElse(SchemaVal.Null)
    }
  }

  implicit def listToSchema[A : ToSchemaVal]: ToSchemaVal[List[A]] = new ToSchemaVal[List[A]]{
    def apply(s: List[A]) = {
      val toSchema = implicitly[ToSchemaVal[A]]
      SchemaVal.SList(s.map(toSchema.apply _))
    }
  }

  implicit def setToSchema[A : ToSchemaVal]: ToSchemaVal[Set[A]] = new ToSchemaVal[Set[A]]{
    def apply(s: Set[A]) = {
      listToSchema[A].apply(s.toList)
    }
  }

  implicit val hnilToSchema: ToSchemaVal[HNil] = new ToSchemaVal[HNil] {
    def apply(l: HNil): SchemaVal = SchemaVal.Empty
  }

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
                                                               implicit
                                                               witness: Witness.Aux[K],
                                                               hEncoder: Lazy[ToSchemaVal[H]],
                                                               tEncoder: ToSchemaVal[T]
                                                             ): ToSchemaVal[FieldType[K, H] :: T] = new ToSchemaVal[FieldType[K, H] :: T]{

    def apply(hlist: FieldType[K, H] :: T): SchemaVal = {
      val fieldName: String = witness.value.name
      val head = hEncoder.value(hlist.head)
      tEncoder(hlist.tail) match{
        case SchemaVal.Obj(v) => SchemaVal.Obj((fieldName, head) :: v)
        case x => throw new IllegalStateException(s"$x is not expected in this position for ${witness.value.name}")
      }
    }

  }

  implicit class ToSchemaOps[A](val a: A) extends AnyVal {
    def toSchemaVal[L <: HList](implicit
                                gen: LabelledGeneric.Aux[A, L],
                                tmr: Lazy[ToSchemaVal[L]]
                               ): SchemaVal = tmr.value(gen.to(a))
  }

  implicit def caseClassToRow[C, R <: HList](implicit
                                             gen: LabelledGeneric.Aux[C, R],
                                             rc: Lazy[ToSchemaVal[R]]
                                            ): ToSchemaVal[C] = new ToSchemaVal[C]{
    def apply(c: C) = rc.value.apply(gen.to(c))
  }

}
