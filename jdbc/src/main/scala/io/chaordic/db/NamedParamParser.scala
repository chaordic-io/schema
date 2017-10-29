package io.chaordic.db

object NamedParamParser{
  type QueryAndParams = (String, List[String])
  def apply(query: String, queryAndParams: QueryAndParams = ("", Nil)): QueryAndParams = {
    val (qry, items) = queryAndParams
    val index = query.indexOf(":")
    if(index >= 0){
      val nextStr = query.substring(0, index) + "?"
      val token = query.substring(index + 1).takeWhile(_.isLetterOrDigit)
      NamedParamParser(query.substring(index + token.length + 1), (qry + nextStr, token :: items))
    }else{
      (qry + query, items.reverse)
    }
  }

}
