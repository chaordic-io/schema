package io.chaordic.prop

import enumeratum.{Enum, EnumEntry}
import org.scalacheck.{Arbitrary, Gen}
import shapeless.{::, Generic, HList, HNil, Lazy}

object Generators {

  implicit val hnilArbitrary: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))

  implicit def hconsArbitrary[V, T <: HList](implicit arbV: Lazy[Arbitrary[V]],
                                                      arbT: Lazy[Arbitrary[T]]): Arbitrary[V :: T] =
    Arbitrary(for{
        v <- arbV.value.arbitrary
        t <- arbT.value.arbitrary
      }yield{
        v :: t
    })

  implicit def caseClassArbitrary[C, R <: HList](implicit
                                                     gen: Generic.Aux[C, R],
                                                     rc: Lazy[Arbitrary[R]]
                                                    ): Arbitrary[C] = Arbitrary(rc.value.arbitrary map gen.from)


  def enumArbitrary[A <: EnumEntry](enum: Enum[A]): Arbitrary[A] = Arbitrary(Gen.oneOf(enum.values))

}
