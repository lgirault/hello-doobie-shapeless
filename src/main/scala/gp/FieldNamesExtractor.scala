package gp

import shapeless._
import shapeless.labelled.FieldType

trait FieldNamesExtractor[T] {
  def fields: List[String] = fields(Nil)

  def fields(acc: List[String]): List[String]
}

object FieldNamesExtractor {

  def apply[T](implicit fieldNameExtractor: FieldNamesExtractor[T]) = fieldNameExtractor


  def instance[T](f: List[String] => List[String]) = new FieldNamesExtractor[T] {
    def fields(acc: List[String]): List[String] = f(acc)
  }

  implicit val hnilExtractor: FieldNamesExtractor[HNil] = instance(acc  => acc.reverse )

  implicit def hlistExtractor[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                          tExtractor: FieldNamesExtractor[T]): FieldNamesExtractor[FieldType[K, H] :: T] =
    instance ( acc => tExtractor.fields(witness.value.name :: acc) )


  implicit def genericExtractor[T, S](implicit gen: LabelledGeneric.Aux[T, S],
                                      extractor: FieldNamesExtractor[S]): FieldNamesExtractor[T] =
    instance ( acc => extractor.fields(acc) )
}
