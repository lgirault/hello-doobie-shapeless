package gp

import doobie.specs2.analysisspec.IOLiteChecker
import doobie.util.iolite.IOLite
import org.specs2.mutable.Specification
import doobie.imports._
import cats.implicits._
import fs2.interop.cats._


case class IhnPerson(id: Long, name: String, age: Option[Short]) extends Ided

case class AnnPerson(@Id id: Long, name: String, age: Option[Short])


object AnalysisTestSpec extends Specification with IOLiteChecker {

  val transactor = DriverManagerTransactor[IOLite](
    "org.hsqldb.jdbc.JDBCDriver", "jdbc:hsqldb:mem:world", "sa", ""
  )


  val drop: Update0 =
    sql"""
        DROP TABLE IF EXISTS person
      """.update

  val dropSeq: Update0 =
    sql"""
        DROP SEQUENCE IF EXISTS person_id_seq
      """.update

  val create: Update0 =
    sql"""
        CREATE TABLE person (
          id   bigint NOT NULL,
          name VARCHAR(254) NOT NULL UNIQUE,
          age  smallint
        )
      """.update


  val createSeq: Update0 =
    sql"""create sequence person_id_seq start with 1 increment by 1""".update


  //hgdb style
  def nextVal: Query0[Option[Int]] =
    sql"call next value for person_id_seq".query[Option[Int]]

  //postgre style
  //  def nextVal =
  //    sql"SELECT nextval('person_id_seq')".query[Int]

  def insert0a(p : AnnPerson): Update0 =
    InsertQueryGenerator.genInsert(p, "person").update

  def insert0b(p : AnnPerson): Update0 =
    InsertQueryGenerator.genMacro(p, "person").update


  def insert1(id: Long, name: String, age: Option[Short]): Update0 =
    sql"insert into person (id, name, age) values ($id, $name, $age)".update


  def select1(id: Long): Query0[IhnPerson] =
    sql"select id, name, age from person where id = $id".query[IhnPerson]


  def insert2(name: String, age: Option[Short]): ConnectionIO[IhnPerson] =
    for {
      id <- nextVal.unique
      _ <- insert1(id.get, name, age).run
      p <- select1(id.get).unique
    } yield p


  def update1(p: IhnPerson): Update0 =
    sql"update person set id = ${p.id}, age = ${p.age}, name = ${p.name} where id = ${p.id}".update


  def update2(p: IhnPerson): Update0 =
    sql"update person set (age, name) = (${p.age}, ${p.name}) where id = ${p.id}".update

  def update3(p: IhnPerson): Update0 =
    UpdateQueryGenerator.genInheritence(p, "person").update

  def update4(p: AnnPerson): Update0 =
    UpdateQueryGenerator.genAnnotation(p, "person").update

  def update5(p: AnnPerson): Update0 =
   UpdateQueryGenerator.genMacro(p, "person").update

  (drop.run *> dropSeq.run *>
    create.run *> createSeq.run).transact(transactor).unsafePerformIO

  checkOutput(nextVal)
  check(insert0a(AnnPerson(36l, "Jerry", None)))
  check(insert0b(AnnPerson(36l, "Jerry", None)))

  check(insert1(36l, "toto", Some(8)))
  check(update1(IhnPerson(36l, "Jerry", None)))
  check(update2(IhnPerson(36l, "Jerry", None)))
  check(update3(IhnPerson(36l, "Jerry", None)))
  check(update4(AnnPerson(36l, "Jerry", None)))
  check(update5(AnnPerson(36l, "Jerry", None)))

//  println(update5(AnnPerson(36l, "Jerry", None))
//  println(UpdateQueryGenerator.genMacro(AnnPerson(36l, "Jerry", None), "turpitude")))

  checkOutput(select1(5l))


}
