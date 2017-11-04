package io.chaordic

import java.sql.{PreparedStatement, ResultSet}
import java.util.UUID

import cats.syntax.either._
import shapeless.{::, Generic, HList, HNil}

package object db {
  implicit val stringToRow: ToRow[String] = new ToRow[String]{
    def apply(s: String, statement: PreparedStatement, index: Int) = statement.setString(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.VARCHAR)
  }
  implicit val intToRow: ToRow[Int] = new ToRow[Int]{
    def apply(s: Int, statement: PreparedStatement, index: Int) = statement.setInt(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.INTEGER)
  }
  implicit val doubleToRow: ToRow[Double] = new ToRow[Double]{
    def apply(s: Double, statement: PreparedStatement, index: Int) = statement.setDouble(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.DOUBLE)
  }

  implicit val longToRow: ToRow[Long] = new ToRow[Long]{
    def apply(s: Long, statement: PreparedStatement, index: Int) = statement.setLong(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.BIGINT)
  }

  implicit val booleanToRow: ToRow[Boolean] = new ToRow[Boolean] {
    def apply(s: Boolean, statement: PreparedStatement, index: Int) = statement.setBoolean(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.BOOLEAN)
  }

  implicit val unitToRow: ToRow[Unit] = new ToRow[Unit]{
    def apply(s: Unit, statement: PreparedStatement, index: Int) = ()
    def setNull(statement: PreparedStatement, index: Int): Unit = ()
  }

  implicit val hnilToRow: ToRow[HNil] = new ToRow[HNil]{
    def apply(s: HNil, statement: PreparedStatement, index: Int) = ()
    override def length: Int = 0
    def setNull(statement: PreparedStatement, index: Int): Unit = ()
  }

  implicit def optionToRow[A : ToRow]: ToRow[Option[A]] = new ToRow[Option[A]]{
    def apply(s: Option[A], statement: PreparedStatement, index: Int) = s.map(v => implicitly[ToRow[A]].apply(v, statement, index)).getOrElse({
      setNull(statement, index)
    })
    def setNull(statement: PreparedStatement, index: Int): Unit = {
      val toRow = implicitly[ToRow[A]]
      toRow.setNull(statement, index)
    }
  }

  implicit def hconsToRow[H: ToRow, T <: HList: ToRow]: ToRow[H :: T] = new ToRow[H :: T]{
    def apply(a: (H :: T), statement: PreparedStatement, index: Int) = {
      a match{
        case (h :: t) => {
          implicitly[ToRow[H]].apply(h, statement, index)
          implicitly[ToRow[T]].apply(t, statement, index + 1)
        }
      }
    }
    def setNull(statement: PreparedStatement, index: Int): Unit = ()

    override def length: Int = {
      implicitly[ToRow[H]].length + implicitly[ToRow[T]].length
    }
  }

  /**
    * enerically derived `ToRow` for case classes.
    * Make sure columns in query are defined in same order as values in case class, as order is significant.
    *
    */
  implicit def caseClassToRow[C, R <: HList](implicit
                                             gen: Generic.Aux[C, R],
                                             rc: ToRow[R]
                                            ): ToRow[C] = new ToRow[C]{
    def apply(c: C, statement: PreparedStatement, index: Int) = rc.apply(gen.to(c), statement, index)
    override def length= rc.length
    def setNull(statement: PreparedStatement, index: Int): Unit = ()
  }



  implicit object FromStringRow extends FromRow[String]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (String, Int)] = {
      Either.right((rs.getString(index), index))
    }
  }

  implicit object FromIntRow extends FromRow[Int]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Int, Int)] = {
      Either.right((rs.getInt(index), index))
    }
  }

  implicit object FromLongRow extends FromRow[Long]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Long, Int)] = {
      Either.right((rs.getLong(index), index))
    }
  }

  implicit object FromDoubleRow extends FromRow[Double]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Double, Int)] = {
      Either.right((rs.getDouble(index), index))
    }
  }

  implicit object FromBooleanRow extends FromRow[Boolean]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Boolean, Int)] = {
      Either.right((rs.getBoolean(index), index))
    }
  }

  implicit object FromUUIDRow extends FromRow[UUID]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (UUID, Int)] = {
      try{
        Either.right((UUID.fromString(rs.getString(index)), index))
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }
  implicit def optionRow[A : FromRow] = new FromRow[Option[A]] {
    def apply(rs: ResultSet, index: Int): Either[Exception, (Option[A], Int)] = {
      Either.right(implicitly[FromRow[A]].apply(rs, index).fold(e => (None, index), r => (Option(r._1), index)))
    }
  }

  implicit val hnilFromRow = new FromRow[HNil]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (HNil, Int)] =  Either.right((HNil, index))
  }

  implicit def hconsFromRow[H: FromRow, T <: HList: FromRow]: FromRow[H :: T] = new FromRow[H :: T]{
    def apply(rs: ResultSet, index: Int): Either[Exception, ((H :: T), Int)] = {
      for{
        a <- implicitly[FromRow[H]].apply(rs, index)
        b <- implicitly[FromRow[T]].apply(rs, index + 1)
      }yield{
        (a._1 :: b._1, b._2)
      }
    }
    def setNull(statement: PreparedStatement, index: Int): Unit = ()
  }

  /**
    * Generically derived `FromRow` for case classes.
    * Make sure columns in result set are defined in same order as values in case class, as order is significant.
    * @param gen
    * @param rc
    * @tparam C
    * @tparam R
    * @return
    */
  implicit def caseClassFromRow[C, R <: HList](implicit
                                               gen: Generic.Aux[C, R],
                                               rc: FromRow[R]
                                              ): FromRow[C] = new FromRow[C]{
    def apply(statement: ResultSet, index: Int) = rc.apply(statement, index).fold(e => Either.left(e), r => Either.right((gen.from(r._1), r._2)))
  }


}
