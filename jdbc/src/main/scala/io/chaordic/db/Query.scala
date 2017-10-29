package io.chaordic.db

import java.sql.Connection

import cats.data.ReaderT
import io.chaordic.io._
import cats.implicits._

object Query{

  def queryFirst[A, B](sql: String, params: A)(implicit ev: ToRow[A], ev2: FromRow[B]): Reader[Connection, Option[B]] = {
    query(sql, params).map(_.headOption)
  }

  def query[A : ToRow, B : FromRow](sql: String, params: A): Reader[Connection, List[B]] = {
    def toResults(rs: java.sql.ResultSet, results: List[B] = Nil): List[B] = {
      if(rs.next()){
        toResults(rs, implicitly[FromRow[B]].apply(rs,1).fold(e => throw e, r => r._1) :: results)
      }else{
        results.reverse
      }
    }

    ReaderT[Safe, Connection, List[B]]{ (c: Connection) =>
      try{
        val pstmt = c.prepareStatement(sql)
        implicitly[ToRow[A]].apply(params, pstmt, 1)
        val rs = pstmt.executeQuery()
        val results = toResults(rs)
        rs.close()
        pstmt.close()
        Either.right(results)
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }

  def executeUpdate[A](sql: String, params: A)(implicit ev: ToRow[A]): Reader[Connection, Int] = {
    ReaderT[Safe, Connection, Int]{ (c: Connection) =>
      try{
        val pstmt = c.prepareStatement(sql)
        ev.apply(params, pstmt, 1)
        val results = pstmt.executeUpdate()
        pstmt.close()
        Either.right(results)
      }catch{
        case e: Exception => Either.left(e)
      }
    }
  }
}
