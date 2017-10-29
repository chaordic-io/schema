package io.chaordic.db

import org.scalatest.{FlatSpec, Matchers}

class NamedParamParserSpec extends FlatSpec with Matchers{

  "NamedParamParser" should "be able find all parameters in a query" in {
    NamedParamParser("SELECT * FROM table WHERE foo=:bar and field= :baz' and three=:gg,")._2 shouldBe(List("bar", "baz", "gg"))
  }

  "NamedParamParser" should "replace all named tokens" in {
    NamedParamParser("SELECT * FROM table WHERE foo=:bar and field= :baz' and three=:gg,")._1 shouldBe("SELECT * FROM table WHERE foo=? and field= ?' and three=?,")
  }

}
