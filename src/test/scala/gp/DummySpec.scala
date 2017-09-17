package gp

import org.specs2.mutable.Specification
import doobie.imports._
import shapeless._
import shapeless.ops.hlist._
import shapeless.labelled.FieldType
import shapeless.ops.hlist._

case class FooBar(id: Long, foo: Int, bar: String) extends Ided

case class FooBaz(@Id
                  id: Long,
                  foo: Int,
                  bar: String)

class DummySpec extends Specification {



  val fooBarList = Generic[FooBar].to(FooBar(37, 5, "toto"))

  val sc = new StringContext("a", "b", "c", "d")

  println(FieldNamesExtractor[FooBar].fields)
  println(sc.fr(fooBarList))

  println(UpdateQueryGenerator.genInheritence(FooBar(36l, 50, "plop"), "foobar"))

  println(IdFinder[FooBaz].find(FooBaz(36, 342, "aye")))
  println(UpdateQueryGenerator.genAnnotation(FooBaz(36, 342, "oy"), "aber"))
  import doobie.imports._

  def update4(p: FooBaz): Update0 =
    UpdateQueryGenerator.genAnnotation(p, "aber").update

  println(update4(FooBaz(36, 342, "tada")))

}
