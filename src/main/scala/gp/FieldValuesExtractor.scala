package gp

import shapeless.{::, HList, HNil, LabelledGeneric, Witness}
import shapeless.labelled.FieldType

trait FieldValuesExtractor[T] {
  def extract(t : T): List[(String, Any)] = extract(t, Nil)

  def extract(t : T, acc: List[(String, Any)]): List[(String, Any)]
}

object FieldValuesExtractor {

  def apply[T](implicit fieldNameExtractor: FieldValuesExtractor[T]) = fieldNameExtractor


  def instance[T](f: (T, List[(String, Any)]) => List[(String, Any)]) = new FieldValuesExtractor[T] {
    def extract(t : T, acc: List[(String, Any)]): List[(String, Any)] = f(t, acc)
  }

  implicit val hnilExtractor: FieldValuesExtractor[HNil] = instance{ case (t, acc)  => acc }

  implicit def hlistExtractor[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                          tExtractor: FieldValuesExtractor[T]): FieldValuesExtractor[FieldType[K, H] :: T] =
    instance { case (hd :: tl, acc) => tExtractor.extract(tl, (witness.value.name, hd) :: acc) }


  implicit def genericExtractor[T, S](implicit gen: LabelledGeneric.Aux[T, S],
                                      extractor: FieldValuesExtractor[S]): FieldValuesExtractor[T] =
    instance { case (t, acc) =>  extractor.extract(gen.to(t), acc) }
}

