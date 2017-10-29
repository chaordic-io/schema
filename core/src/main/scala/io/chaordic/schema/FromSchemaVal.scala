package io.chaordic.schema


import java.util.UUID

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import io.chaordic.schema.SchemaVal.{Null, Obj}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{FieldType, field}
import enumeratum.{Enum, EnumEntry}

import scala.util.Try


case class ValidationError(msg: ValidationMessage, path: List[String])

sealed trait ValidationMessage
case class FormatError(msg: String) extends ValidationMessage
case object NullOrMissingFieldError extends ValidationMessage

trait FromSchemaVal[A]{
  def apply(value: SchemaVal, path: List[String]): Validated[NonEmptyList[ValidationError],A]
}

trait FromStrSchemaVal[A] extends FromSchemaVal[A]{
  def apply(value: SchemaVal, path: List[String]): Validated[NonEmptyList[ValidationError], A] = value match {
    case SchemaVal.Str(s) => fromStr(s, path)
    case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
    case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid String"), path), Nil))
  }

  def fromStr(s: String, path: List[String]): Validated[NonEmptyList[ValidationError], A]
}

object FromSchemaVal{

  implicit class FromResultOps[A](val o: Option[A]){
    def toResult(message: String, path: List[String]): Validated[NonEmptyList[ValidationError],A] = {
      o.map(Valid.apply).getOrElse(Invalid(NonEmptyList(ValidationError(FormatError(message), path), Nil)))
    }
  }

  implicit class ValidatedOps[A,E](fst: Validated[NonEmptyList[E],A]){
    def combineMap[B, C](snd: Validated[NonEmptyList[E],B])(fn: (A,B) => C): Validated[NonEmptyList[E], C] ={
      (fst, snd) match{
        case (Valid(a), Valid(b)) => Valid(fn(a,b))
        case (Valid(a), Invalid(b)) => Invalid(b)
        case (Invalid(a), Valid(b)) => Invalid(a)
        case (Invalid(a), Invalid(b)) => Invalid(NonEmptyList(a.head, a.tail ++ b.toList))
      }
    }
  }

  implicit val stringFromSchema: FromSchemaVal[String] = new FromStrSchemaVal[String]{
    override def fromStr(s: String, path: List[String]) = Valid(s)
  }

  implicit val bigDecimalFromSchema: FromSchemaVal[BigDecimal] = new FromSchemaVal[BigDecimal]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.IntNum(s) => Valid(BigDecimal(s.toString))
      case SchemaVal.LongNum(s) => Valid(BigDecimal(s.toString))
      case SchemaVal.DoubleNum(s) => Valid(BigDecimal(s.toString))
      case SchemaVal.Str(s) => Try(BigDecimal(s)).toOption.toResult(s"$s is not a valid Decimal number", path)
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Decimal number"), path), Nil))
    }
  }

  implicit val uuidFromSchema: FromSchemaVal[UUID] = new FromStrSchemaVal[UUID]{
    override def fromStr(s: String, path: List[String]) = Try(UUID.fromString(s)).toOption.toResult(s"$s is not a valid UUID", path)
  }

  implicit def mapFromSchema[A : FromSchemaVal]: FromSchemaVal[Map[String, A]] = new FromSchemaVal[Map[String, A]] {
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.Obj(s) => {
       ???
      }
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Decimal number"), path), Nil))
    }
  }

  implicit val intToSchema: FromSchemaVal[Int] = new FromSchemaVal[Int]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.IntNum(s) => Valid(s)
      case SchemaVal.LongNum(s) => Valid(s.toInt)
      case SchemaVal.DoubleNum(s) => Valid(s.toInt)
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Int"), path), Nil))
    }
  }
  implicit val doubleToSchema: FromSchemaVal[Double] = new FromSchemaVal[Double]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.DoubleNum(s) => Valid(s)
      case SchemaVal.IntNum(s) => Valid(s.toDouble)
      case SchemaVal.LongNum(s) => Valid(s.toDouble)
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case SchemaVal.Str(s) => {
        if(s.forall(s => s.isDigit || s == '.' || s == '-')){
          Valid(s.toDouble)
        }else{
          Invalid(NonEmptyList(ValidationError(FormatError(s"$s is not a numeric value"), path), Nil))
        }
      }
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Double"), path), Nil))
    }
  }

  implicit val longToSchema: FromSchemaVal[Long]= new FromSchemaVal[Long]{
    def apply(s: SchemaVal, path: List[String])  = s match{
      case SchemaVal.LongNum(s) => Valid(s)
      case SchemaVal.IntNum(s) => Valid(s.toLong)
      case SchemaVal.DoubleNum(s) => Valid(s.toLong)
      case SchemaVal.Str(s) => {
        if(s.forall(s => s.isDigit || s == '.' || s == '-')){
          Valid(s.toLong)
        }else{
          Invalid(NonEmptyList(ValidationError(FormatError(s"$s is not a numeric value"), path), Nil))
        }
      }
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Long"), path), Nil))
    }
  }

  implicit val booleanToSchema: FromSchemaVal[Boolean] = new FromSchemaVal[Boolean]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.Bool(s) => Valid(s)
      case SchemaVal.Str(s) => {
        s.toLowerCase match{
          case "on" => Valid(true)
          case "true" => Valid(true)
          case "false" => Valid(false)
          case "t" => Valid(true)
          case "f" => Valid(false)
        }
      }
      case Null => Valid(false)
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid Boolean"), path), Nil))
    }
  }

  implicit def schemaToList[A : FromSchemaVal]: FromSchemaVal[List[A]] = new FromSchemaVal[List[A]]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.SList(s) => {
        val fromSchemaVal = implicitly[FromSchemaVal[A]]
        val mapped = s.map(f => fromSchemaVal(f, path))
        if(mapped.forall(_.isValid)){
          Valid(mapped.flatMap({
            case Valid(ok) => List(ok)
            case _ => Nil
          }))
        }else{
          Invalid(NonEmptyList(ValidationError(FormatError("Errors in mapping from list"), path),mapped.flatMap({
            case fe: Invalid[NonEmptyList[ValidationError]] => fe.e.toList
            case _ => Nil
          })))
        }
      }
      case SchemaVal.Str(str) => {
        val fromSchemaVal = implicitly[FromSchemaVal[A]]
        fromSchemaVal.apply(SchemaVal.Str(str), path).map(s => List(s))
      }
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err is not a valid List"), path), Nil))
    }
  }

  implicit def schemaToSet[A : FromSchemaVal]: FromSchemaVal[Set[A]] = new FromSchemaVal[Set[A]]{
    def apply(s: SchemaVal, path: List[String]) = schemaToList[A].apply(s, path).map(_.toSet)
  }

  implicit def schemaToOpt[A : FromSchemaVal]: FromSchemaVal[Option[A]] = new FromSchemaVal[Option[A]]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case Null => Valid(None)
      case s => implicitly[FromSchemaVal[A]].apply(s, path).map(res => Some(res))
    }
  }

  implicit val hnilToVal = new FromSchemaVal[HNil]{
    def apply(s: SchemaVal, path: List[String]) =  Valid(HNil)
  }


  implicit def valToHCons[K <: Symbol, V, T <: HList](implicit
                                                         witness: Witness.Aux[K],
                                                         fromMapV: Lazy[FromSchemaVal[V]],
                                                         fromMapT: Lazy[FromSchemaVal[T]]
  ): FromSchemaVal[FieldType[K, V] :: T] = new FromSchemaVal[FieldType[K, V] :: T] {
    def apply(s: SchemaVal, path: List[String]) = {
      s match {
        case obj: Obj => {
          val newPath: List[String] = (witness.value.name :: path).reverse
          val headResult = obj.get(witness.value.name).map(fromMapV.value(_, newPath)).
            getOrElse(fromMapV.value(Null, newPath))

          (headResult combineMap fromMapT.value(obj, path)) {(h, t) => field[K](h) :: t}
        }
        case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
        case f => Invalid(NonEmptyList(ValidationError(FormatError(s"$f did not match required input of Obj"), path), Nil))
      }
    }
  }


  implicit def caseClassFromSchemaVal[C, R <: HList](implicit
                                               gen: LabelledGeneric.Aux[C, R],
                                               rc: Lazy[FromSchemaVal[R]]
                                              ): FromSchemaVal[C] = new FromSchemaVal[C]{
    def apply(s: SchemaVal, path: List[String]) = rc.value.apply(s, path) map gen.from
  }

  implicit class FromSchemaOps[L <: HList](val sValue: SchemaVal) extends AnyVal {
    def fromSchemaVal[A](implicit
                                gen: LabelledGeneric.Aux[A, L],
                                tmr: Lazy[FromSchemaVal[L]]
                               ): Validated[NonEmptyList[ValidationError],A] = tmr.value.apply(sValue, Nil) map gen.from
  }

  def enumFromSchema[A <: EnumEntry](enum: Enum[A]): FromSchemaVal[A] = new FromSchemaVal[A]{
    def apply(s: SchemaVal, path: List[String]) = s match{
      case SchemaVal.Str(s) => enum.withNameInsensitiveOption(s).toResult(s"$s did not match any entry in ${enum.values}", path)
      case Null => Invalid(NonEmptyList(ValidationError(NullOrMissingFieldError, path), Nil))
      case err => Invalid(NonEmptyList(ValidationError(FormatError(s"$err does not match expected enum type"), path), Nil))
    }
  }

}
