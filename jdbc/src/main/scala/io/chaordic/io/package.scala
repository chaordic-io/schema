package io.chaordic
import cats.data.{Kleisli, ReaderT}
import cats.syntax.either._
import cats.instances.either._

package object io {

  type Safe[A] = Either[Exception, A]
  type Reader[CONF,A] = ReaderT[Safe, CONF, A]

  def getConf[A]: Reader[A, A] = Kleisli.ask[Safe, A]

  object Reader{
    def apply[CONF, A](f: CONF => Safe[A]): Reader[CONF, A] = Kleisli.apply[Safe, CONF, A](f)
    def lift[CONF, A](a: => A): Reader[CONF, A] = ReaderT.apply[Safe, CONF, A]{ (c: CONF) =>
      try{
        Either.right(a)
      }catch{
        case e: Exception => Either.left(e)
      }
    }

    def sequence[CONF, A](list: List[Reader[CONF,A]]): Reader[CONF, List[A]] = {
      list.foldRight[Reader[CONF, List[A]]](Reader.lift[CONF, List[A]](Nil))({ (value, acc) =>
        for{
          r <- value
          list <- acc
        }yield{
          r :: list
        }
      })
    }
  }

  implicit class IOReaderOps[CONF, A](reader: Reader[CONF,A]){
    def map[B](f: A => B): Reader[CONF, B] = {
      for{
        v <- reader
      }yield{
        f(v)
      }
    }
  }

  implicit class SafeOps[A](safe: Safe[A]){
    def lift[CONF]: Reader[CONF,A] = ReaderT.lift(safe)
  }
}
