package gp

import doobie.imports.Fragment
import doobie.util.param.Param
import shapeless.{Generic, Poly1, Witness}
import doobie.syntax.string._
import shapeless.{:: => `:hlist:`, _}

import scala.annotation.StaticAnnotation


trait Ided {
  def id: Long
}

case class Id() extends StaticAnnotation

object UpdateQueryGenerator {


  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  def genMacro_impl[T: c.WeakTypeTag](c: Context)(t: c.Tree, tableName: c.Expr[String]): c.Expr[Fragment] = {
    import c.universe._

    //    val method = c.weakTypeOf[T].decls.filterNot(_.isTerm).head
    //    val call = c.Expr( Select(t.tree, method.name) )
    //    println("call => " + call)
    val (fieldNames, fieldCalls) = c.weakTypeOf[T].decls.filter(s => s.isTerm && !s.isMethod).map { s =>
      (s.name, c.Expr(Select(t, s.name)))
      //    (s.name,
      //        c.Expr( Select(Select(t, TypeApply(q"asInstanceOf"), c.weakTypeOf[T] ))
      //c.Expr(Select(q""" $t.asInstanceOf[${c.weakTypeOf[T]}]""", s.name)))
    }.unzip

    val sc = c.freshName(TermName("temp"))

    println(fieldNames)
    println(fieldCalls)
    val emptyString = Literal(Constant(""))
    val q0 = fieldNames match {
      case Nil => ???
      case f :: Nil => List(Literal(Constant(s"$f = ")), emptyString)
      case f :: fs =>

        val fsProcessed = fs.foldRight(List(emptyString)) {
          case (f0, acc) => Literal(Constant(s", $f0 = ")) :: acc
        }

        Literal(Constant(s"$f = ")) :: fsProcessed

    }

    val updateFragment = q""" new StringContext("update ", " set ").s($tableName)"""

    c.Expr[Fragment](
      q"""{
         val $sc = new StringContext($updateFragment + ${q0.head}, ..${q0.tail})
         $sc.fr(..$fieldCalls)
         }
         """)


  }

  def genMacro[T](t: T, tableName: String): Fragment = macro genMacro_impl[T]


  def genUpdateFragment[T, S](t: T, tableName: String)
                             (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                              generic: Generic.Aux[T, S],
                              param: Param[S]): Fragment = {
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
    sc.fr(generic.to(t))
  }

  def genInheritence[T <: Ided, S](t: T, tableName: String)
                                  (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                                   generic: Generic.Aux[T, S],
                                   param: Param[S])= {

    val whereClause: Fragment = fr"where id = ${t.id}"

    genUpdateFragment(t, tableName) ++ whereClause
  }


  def genAnnotation[T, S, IdT](t: T, tableName: String)
                              (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                               generic: Generic.Aux[T, S],
                               param: Param[S],
                               finder: IdFinder.Aux[T, IdT],
                               param2: Param[IdT `:hlist:` HNil]) = {

    val (idFieldName, idValue) = finder.find(t)

    val whereClause0 = new StringContext(s"where $idFieldName = ", "")
    val whereClause: Fragment = {
      import shapeless.::
      whereClause0.fr(idValue :: HNil)
    }

    genUpdateFragment(t, tableName) ++ whereClause
  }


}
