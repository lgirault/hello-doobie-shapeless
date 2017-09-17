package gp

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{CollectFirst, Zip}

object IdFieldIdentifier extends Poly1 {
  implicit def id[K <: Symbol, H](implicit witness: Witness.Aux[K]): IdFieldIdentifier.Case.Aux[(FieldType[K, H], Some[Id]), (String, H)] =
    at[(FieldType[K, H], Some[Id])] { case (h, _) => (witness.value.name, h) }
}


trait IdFinder[T] {
  type Out

  def find(t: T): (String, Out)
}

object IdFinder {

  type Aux[T, Out0] = IdFinder[T] {type Out = Out0}

  def apply[T](implicit finder: IdFinder[T]): IdFinder[T] = finder


  implicit def genFinder[T, S <: HList, S2 <: HList, Z <: HList, Out0, H](implicit gen: LabelledGeneric.Aux[T, S],
                                                                          idAnnotations: Annotations.Aux[Id, T, S2],
                                                                          zipper: Zip.Aux[S :: S2 :: HNil, Z],
                                                                          collectFirst: CollectFirst.Aux[Z, IdFieldIdentifier.type, Out0],
                                                                          ev: Out0 <:< (String, H)): IdFinder.Aux[T, H] =
    new IdFinder[T] {
      type Out = H
      def find(t: T): (String, Out) = gen.to(t).zip(idAnnotations.apply()).collectFirst(IdFieldIdentifier)
    }

}