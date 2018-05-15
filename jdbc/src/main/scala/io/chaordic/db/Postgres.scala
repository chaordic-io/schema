package io.chaordic.db

import java.sql.{PreparedStatement, ResultSet}
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.{Map => JMap}

import scala.collection.JavaConverters._
import cats.syntax.either._
import io.circe.Json
import org.postgresql.util.PGobject
import io.circe.parser._

case object Postgres extends Dialect{
  def metadataBool(s: String): Boolean = {
    s.toLowerCase.trim match{
      case "yes" => true
      case "no" => false
    }
  }

  // This should never be an implicit - it's catchment will be far too wide.
  def listRow[A] = new FromRow[List[A]]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (List[A], Int)] = {
      try{
        val array = rs.getArray(index).getArray()
        val strings = array.asInstanceOf[Array[A]]
        Either.right((strings.toList, index))
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }

  implicit def stringListRow = listRow[String]
  implicit def uuidRow = listRow[UUID]
  implicit def boolRow = listRow[Boolean]
  implicit def intRow = listRow[Int]
  implicit def longRow = listRow[Long]

  implicit object FromLocalDateTime extends FromRow[LocalDateTime]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (LocalDateTime, Int)] = {
      try {
        Either.right((rs.getTimestamp(index).toLocalDateTime(), index))
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }

  implicit object FromLocalDate extends FromRow[LocalDate]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (LocalDate, Int)] = {
      try {
        Either.right((rs.getDate(index).toLocalDate(), index))
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }

  implicit object FromJsonRow extends FromRow[Json]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Json, Int)] = {
      parse(rs.getString(index)).fold(s => Either.left(new IllegalStateException(s"Could not parse $s")), r => Either.right((r, index)))
    }
  }

  implicit object FromStringMapRow extends FromRow[Map[String, String]]{
    def apply(rs: ResultSet, index: Int): Either[Exception, (Map[String,String], Int)] = {
      try{
        val result = rs.getObject(index).asInstanceOf[JMap[Any, Any]]
        Either.right((result.keySet().asScala.toList.foldLeft(Map[String,String]())({(acc, value) =>
          acc ++ Map(value.toString -> result.get(value).toString)
        }), index))
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }
  implicit val localDateTimeToRow: ToRow[LocalDateTime] = new ToRow[LocalDateTime]{
    def apply(s: LocalDateTime, statement: PreparedStatement, index: Int) = statement.setTimestamp(index, java.sql.Timestamp.valueOf(s))
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.TIMESTAMP)
  }

  implicit val localDateToRow: ToRow[LocalDate] = new ToRow[LocalDate]{
    def apply(s: LocalDate, statement: PreparedStatement, index: Int) = statement.setDate(index, java.sql.Date.valueOf(s))
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.DATE)
  }

  implicit val uuidToRow: ToRow[UUID] = new ToRow[UUID]{
    def apply(s: UUID, statement: PreparedStatement, index: Int) = statement.setObject(index, s)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.JAVA_OBJECT)
  }

  implicit val mapToRow: ToRow[Map[String,String]] = new ToRow[Map[String,String]]{
    def apply(s: Map[String,String], statement: PreparedStatement, index: Int) = statement.setObject(index, s.asJava)
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.JAVA_OBJECT)
  }

  implicit val jsonToRow: ToRow[Json] = new ToRow[Json]{
    def apply(s: Json, statement: PreparedStatement, index: Int) = {
      val jsonObject = new PGobject()
      jsonObject.setType("json")
      jsonObject.setValue(s.noSpaces)
      statement.setObject(index, jsonObject)
    }
    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.CLOB) // probably wrong?
  }

  implicit val arrayToRow: ToRow[List[String]] = new ToRow[List[String]]{
    def apply(s: List[String], statement: PreparedStatement, index: Int) = {
      val arr = statement.getConnection.createArrayOf("text", s.toArray)
      statement.setArray(index, arr)
    }

    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.ARRAY)
  }

  implicit val uuidArrayToRow: ToRow[List[UUID]] = new ToRow[List[UUID]]{
    def apply(s: List[UUID], statement: PreparedStatement, index: Int) = {
      val arr = statement.getConnection.createArrayOf("uuid", s.toArray)
      statement.setArray(index, arr)
    }

    def setNull(statement: PreparedStatement, index: Int): Unit = statement.setNull(index, java.sql.Types.ARRAY)
  }


}
/*
serial
float4
point
int8
bit
path
polygon
timestamp
bigserial
circle
json
cidr
line
txid_snapshot
uuid
hstore
tsvector
pg_lsn
text
macaddr
float8
bytea
tsquery
bpchar
interval
int4
bool
date
xml
inet
money
varbit
_uuid
box
varchar
numeric
time
int2
lseg
jsonb
 */

/*
numeric_col numeric
text_col text
smallint_col int2
bigint_col int8
bytea_col bytea
macaddr_col macaddr
polygon_col polygon
txid_snapshot_col txid_snapshot
integer_col int4
bigserial_col bigserial
bitv_col varbit
json_col json
inet_col inet
uuid_array _uuid
characterv_col varchar
point_col point
tsquery_col tsquery
boolean_col bool
pg_lsn_col pg_lsn
character_col bpchar
line_col line
cidr_col cidr
date_col date
box_col box
smallserial_col int2
bit_col bit
uuid_col uuid
path_col path
id serial
circle_col circle
interval_col interval
lseg_col lseg
hstore_column hstore
jsonb_col jsonb
timestamp_col timestamp
time_col time
money_col money
xml_col xml
double_col float8
real_col float4
tsvector_col tsvector
 */