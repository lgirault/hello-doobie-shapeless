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
    val clazz = c.weakTypeOf[T]
    val (fieldNames, fieldCalls) = clazz.decls.collect{
      case m : MethodSymbol if m.isCaseAccessor =>
        (m.name, c.Expr(Select(t, m.name)))
    }.unzip




    val idParam = symbolOf[T].asClass.primaryConstructor.typeSignature.paramLists.collect{
      case l => l.find{
        s => s.annotations.exists{ a =>
          a.tree.tpe == symbolOf[Id].asType.toType
        }
      }
    }.head.head

    val idGet = clazz.decls.collect{
      case m : MethodSymbol if m.isCaseAccessor && m.name == idParam.name =>
        c.Expr(Select(t, m.name))
    }.head

    val sc = c.freshName(TermName("temp"))


    val whereClause = Literal(Constant(s" where ${idParam.name} = "))

    val emptyString = Literal(Constant(""))
    val q0 = fieldNames match {
      case Nil => ???
      case f :: Nil => List(Literal(Constant(s"$f = ")), whereClause, emptyString)
      case f :: fs =>

        val fsProcessed = fs.foldRight(List(whereClause, emptyString)) {
          case (f0, acc) => Literal(Constant(s", $f0 = ")) :: acc
        }

        Literal(Constant(s"$f = ")) :: fsProcessed

    }

    val Literal(Constant(h)) = q0.head

    val updateFragment =  Literal(Constant(s"update ${c.eval[String](tableName)} set $h"))

    c.Expr[Fragment](
      q"""{
         val $sc = new StringContext($updateFragment, ..${q0.tail})
         $sc.fr(..$fieldCalls, $idGet)
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
      whereClause0.fr(HNil.::(idValue))
    }

    genUpdateFragment(t, tableName) ++ whereClause
  }


}
