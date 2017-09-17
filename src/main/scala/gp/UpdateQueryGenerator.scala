package gp

import doobie.imports.Fragment
import doobie.util.param.Param
import shapeless.{Generic, Poly1, Witness}
import doobie.syntax.string._
import shapeless.{:: => `:hlist:`, _}
import shapeless.ops.hlist._
import shapeless.labelled.FieldType
import shapeless.ops.hlist._

import scala.annotation.StaticAnnotation


trait Ided {
  def id: Long
}

case class Id() extends StaticAnnotation

object UpdateQueryGenerator {

  def genInheritence[T <: Ided, S](t: T, tableName: String)
                                  (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                                   generic: Generic.Aux[T, S],
                                   param: Param[S]) = {

    val hlist = generic.to(t)

    val whereClause: Fragment = fr"where id = ${t.id}"

    val q0 = fieldNamesExtractor.fields match {
      case Nil => ???
      case f :: Nil => List(s"$f = ", "")
      case f :: fs =>

        val fsProcessed = fs.foldRight(List("")) {
          case (f0, acc) => s", $f0 = " :: acc
        }

        s"$f = " :: fsProcessed

    }

    val sc = new StringContext(s"update $tableName set " + q0.head :: q0.tail: _*)
    sc.fr(hlist) ++ whereClause
  }


  def genAnnotation[T, S, IdT](t: T, tableName: String)
                              (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                               generic: Generic.Aux[T, S],
                               param: Param[S],
                               finder: IdFinder.Aux[T, IdT],
                               param2: Param[IdT `:hlist:` HNil]) = {

    val (idFieldName, idValue) = finder.find(t)
    val hlist = generic.to(t)

    val whereClause0 = new StringContext(s"where $idFieldName = ", "")
    val whereClause: Fragment = {
      import shapeless.::
      whereClause0.fr(idValue :: HNil)
    }

    val q0 = fieldNamesExtractor.fields match {
      case Nil => ???
      case f :: Nil => List(s"$f = ", "")
      case f :: fs =>

        val fsProcessed = fs.foldRight(List("")) {
          case (f0, acc) => s", $f0 = " :: acc
        }

        s"$f = " :: fsProcessed

    }

    val sc = new StringContext(s"update $tableName set " + q0.head :: q0.tail: _*)
    sc.fr(hlist) ++ whereClause
  }


}
