package io.chaordic.db

import javax.sql.DataSource

/**
  * This is a configuration object that is relatively expensive to construct, only instantiate once if possible!
  * @param dataSource
  * @param dialect
  */
case class Db(dataSource: DataSource, dialect: Dialect) {

  private def toTableNames(rs: java.sql.ResultSet, results: List[String] = Nil): List[String] = {
    if(rs.next()){
      toTableNames(rs, rs.getString("TABLE_NAME") :: results)
    }else{
      results.reverse
    }
  }

  private def toColumns(rs: java.sql.ResultSet, results: List[Column] = Nil): List[Column] = {
    if(rs.next()){
      val col = Column(rs.getString("COLUMN_NAME"),
        rs.getString("TYPE_NAME"),
        rs.getInt("DATA_TYPE"),
        rs.getInt("COLUMN_SIZE"),
        rs.getInt("DECIMAL_DIGITS"),
        dialect.metadataBool(rs.getString("IS_NULLABLE")),
        dialect.metadataBool(rs.getString("IS_AUTOINCREMENT")))

      toColumns(rs, col :: results)
    }else{
      results.reverse
    }
  }

  private val conn = dataSource.getConnection
  private val metadata = conn.getMetaData()
  private val resultSet = metadata.getTables(null, null, null, Array[String]("TABLE"))
  private val tableNames = toTableNames(resultSet)

  val tables = tableNames.map(tableName => Table(tableName,toColumns(metadata.getColumns(null, null, tableName, null))))

  conn.close()

}
case class Table(name: String, columns: List[Column])

case class Column(columnName: String,
                  typeName: String,
                  dataType: Int,
                  columnSize: Int,
                  decimalDigits: Int,
                  isNullable: Boolean,
                  isAutoIncrement: Boolean
                 )

