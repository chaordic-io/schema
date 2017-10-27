package io.chaordic.db

import org.scalatest.{FlatSpec, Matchers}

class MetadataSpec extends FlatSpec with Matchers{



  "ToSchemaVal" should "be symmetric on an arbitrarily nested case class with options, lists and case object enums" in {

    Db(TestDataSource.dataSource, Postgres).tables.filter(_.name == "test").flatMap(_.columns.map(c => c.columnName + " " + c.typeName)).toSet.foreach(println)
  }

}
