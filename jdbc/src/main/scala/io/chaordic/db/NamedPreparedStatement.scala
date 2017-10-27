package io.chaordic.db

import java.sql.{Connection, ResultSet}

import io.chaordic.schema.SchemaVal

class NamedPreparedStatement(con: Connection) {

  def executeQuery(sql: String, params: SchemaVal*): ResultSet = {
    ???
  }

  def executeUpdate(sql: String, params: SchemaVal*): Int = {
    ???
  }

}
