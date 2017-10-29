package io.chaordic.db

import java.sql.ResultSet

trait FromRow[A]{
  def apply(rs: ResultSet, index: Int): Either[Exception, (A, Int)]
}
