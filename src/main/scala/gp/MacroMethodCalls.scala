package gp

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object MacroMethodCalls {

  def printCall_impl1[T : c.WeakTypeTag](c : Context)(t : c.Expr[T]) : c.Expr[Unit] = {
    import c.universe._
    val m1 = c.weakTypeOf[T].decls.filter(d => d.isMethod && d.asMethod.isCaseAccessor).head
    val s1 = Literal(Constant(s"t.${m1.name} = "))
    val v1 = c.Expr( Select(t.tree, m1.name) )


    val sc = q""" new StringContext($s1, "").s($v1)"""


    c.Expr[Unit](q""" println($sc) """)
  }

  def printCall1[T](t : T) : Unit = macro printCall_impl1[T]


  def printCall_impl2[T : c.WeakTypeTag](c : Context)(t : c.Expr[T]) : c.Expr[Unit] = {
    import c.universe._
    val m1 = c.weakTypeOf[T].decls.filter(d => d.isMethod && d.asMethod.isCaseAccessor).head
    val m2 = c.weakTypeOf[T].decls.filter(d => d.isMethod && d.asMethod.isCaseAccessor).tail.head
    val s1 = Literal(Constant(s"t.${m1.name} = "))
    val v1 = c.Expr( Select(t.tree, m1.name) )
    val s2 = Literal(Constant(s"t.${m2.name} = "))
    val v2 = c.Expr( Select(t.tree, m2.name) )

    val sc = q""" new StringContext($s1, $s2 , "").s($v1, $v2)"""


    c.Expr[Unit](q""" println($sc) """)
  }

  def printCall2[T](t : T) : Unit = macro printCall_impl2[T]


  def printCallList_impl[T : c.WeakTypeTag](c : Context)(t : c.Expr[T]) : c.Expr[Unit] = {
    import c.universe._
    val (ss, vs) = c.weakTypeOf[T].decls.collect {
      case value : MethodSymbol if value.isCaseAccessor =>
        (Literal(Constant(s" t.${value.name} = ")), c.Expr( Select(t.tree, value.name) ) )
    } .unzip

    val sc = q""" new StringContext(..$ss , "").s(..$vs)"""

    c.Expr[Unit](q""" println($sc) """)
  }


  def printCallList[T](t : T) : Unit = macro printCallList_impl[T]

}
