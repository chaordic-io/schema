package io.chaordic.db

trait Dialect {
  def metadataBool(s: String): Boolean


}
