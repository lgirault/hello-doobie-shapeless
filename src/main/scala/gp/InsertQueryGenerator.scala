package gp

import doobie.imports.Fragment
import doobie.util.param.Param
import shapeless.{Generic, Poly1, Witness}
import doobie.syntax.string._
import shapeless.{:: => `:hlist:`, _}

import scala.annotation.StaticAnnotation


object InsertQueryGenerator {


  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  def genMacro_impl[T: c.WeakTypeTag](c: Context)(t: c.Tree, tableName: c.Expr[String]): c.Expr[Fragment] = {
    import c.universe._

    val clazz = c.weakTypeOf[T]
    val (fieldNames, fieldCalls) = clazz.decls.collect{
      case m : MethodSymbol if m.isCaseAccessor =>
        (m.name, c.Expr(Select(t, m.name)))
    }.unzip


    val sc = c.freshName(TermName("temp"))



    val q0 = fieldNames.mkString(s"insert into ${c.eval(tableName)} (", ",", ") values (")

    val closingPar = List(Literal(Constant(")")))
    val commas  = fieldNames.tail match {
      case Nil => closingPar
      case l =>
        (for (_ <- 0 until l.size ) yield Literal(Constant(","))) ++ closingPar
    }

    val updateFragment =  Literal(Constant(q0))

    c.Expr[Fragment](
      q"""{
         val $sc = new StringContext($updateFragment, ..$commas)
         $sc.fr(..$fieldCalls)
         }
         """)


  }

  def genMacro[T](t: T, tableName: String): Fragment = macro genMacro_impl[T]


  def genInsert[T, S](t: T, tableName: String)
                             (implicit fieldNamesExtractor: FieldNamesExtractor[T],
                              generic: Generic.Aux[T, S],
                              param: Param[S]): Fragment = {
    val fields = fieldNamesExtractor.fields
    val q0 = fields.mkString(s"insert into $tableName (", ",", ") values (")

    val commas = fields.tail match {
      case Nil => List(")")
      case l =>  List(")").reverse_:::(l.map(_ => ","))
    }


    val sc = new StringContext(q0 :: commas:_*)
    sc.fr(generic.to(t))
  }

}
