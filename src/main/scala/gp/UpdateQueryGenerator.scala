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


  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  def genMacro_impl[T : c.WeakTypeTag](c: Context)(t : c.Expr[T], tableName : c.Expr[String]) : c.Expr[Fragment] = {
    import c.universe._

    val (fieldNames, fieldCalls) = c.weakTypeOf[T].decls.filterNot(_.isMethod).map { s => 
      (s.name, q"${t.value}.${s.name}")
    }.unzip
    
    val sc = c.freshName(TermName("temp"))

    val q0 = fieldNames match {
      case Nil => ???
      case f :: Nil => q""" "${f.name} = ", :: "" """
      case f :: fs =>

        val fsProcessed = fs.foldRight(List("")) {
          case (f0, acc) => s", $f0 = " :: acc
        }

        s"$f = " :: fsProcessed

    }

    c.Expr[Fragment](
        q"""{
         val $sc = new StringContext("update ${tableName.value} set ")
         $sc.fr()
         }
         """ )
       

}


  def genMacro[T](t: T, tableName: String): Fragment = macro genMacro_impl[T]


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
