package io.chaordic.db

import java.sql.PreparedStatement

trait ToRow[A]{
  def apply(row: A, statement: PreparedStatement, index: Int): Unit

  def length: Int = {
    1
  }
}
