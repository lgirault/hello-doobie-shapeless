package gp

import doobie.specs2.analysisspec.IOLiteChecker
import doobie.util.iolite.IOLite
import org.specs2.mutable.Specification
import doobie.imports._
import cats.implicits._
import fs2.interop.cats._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.rng.Seed
import org.specs2.mutable.Specification

/*

import org.scalacheck.Gen._


import scala.collection.mutable*/

case class TestedResource(@Id i1: Int,
                          i2: Int,
                          i3: Int,
                          i4: Int,
                          i5: Int,
                          s1: String,
                          s2: String,
                          s3: String,
                          s4: String,
                          s5: String,
                          d1: Double,
                          d2: Double,
                          d3: Double,
                          d4: Double,
                          d5: Double
                         )

object BenchmarkTestValuesGenerator {

  val arbitraryTestedResource : Gen[TestedResource] = arbitrary[TestedResource]

  def randomWithId(id : Int) : TestedResource = {
    arbitraryTestedResource.pureApply(Gen.Parameters.default, Seed(id)).copy(i1 = id)
  }
  def generate(n : Int = 1000): List[TestedResource] =
    Gen.listOfN(n, arbitraryTestedResource).pureApply(Gen.Parameters.default, Seed(30))

  val drop: Update0 =
    sql"""
        DROP TABLE IF EXISTS test
      """.update

  val dropSeq: Update0 =
    sql"""
        DROP SEQUENCE IF EXISTS test_id_seq
      """.update


  val create: Update0 =
    sql"""
        CREATE TABLE test (
          i1 int NOT NULL UNIQUE,
          i2 int NOT NULL,
          i3 int NOT NULL,
          i4 int NOT NULL,
          i5 int NOT NULL,
          s1 VARCHAR(254) NOT NULL,
          s2 VARCHAR(254) NOT NULL,
          s3 VARCHAR(254) NOT NULL,
          s4 VARCHAR(254) NOT NULL,
          s5 VARCHAR(254) NOT NULL,
          d1 double NOT NULL,
          d2 double NOT NULL,
          d3 double NOT NULL,
          d4 double NOT NULL,
          d5 double NOT NULL
        )
      """.update

  val createSeq: Update0 =
    sql"""create sequence test_id_seq start with 1 increment by 1""".update


  //hgdb style
  def nextVal: Query0[Option[Int]] =
    sql"call next value for test_id_seq".query[Option[Int]]

  def select1(id: Long): Query0[TestedResource] =
    sql"select i1,i2,i3,i4,i5,s1,s2,s3,s4,s5,d1,d2,d3,d4,d5  from test where i1 = $id".query[TestedResource]

  def insertShapeless(p : TestedResource): ConnectionIO[TestedResource] =
    for {
      id <- nextVal.unique
      _ <- InsertQueryGenerator.genInsert(p.copy(i1=id.get), "test").update.run
      p <- select1(id.get).unique
    } yield p


  def insertMacro(p : TestedResource): ConnectionIO[TestedResource] =
    for {
      id <- nextVal.unique
      _ <- InsertQueryGenerator.genMacro(p.copy(i1=id.get), "test").update.run
      p <- select1(id.get).unique
    } yield p


  def updateShapelessAnnotation(tr : TestedResource) : Update0 =
    UpdateQueryGenerator.genAnnotation(tr, "test").update

  def updateMacro(tr: TestedResource): Update0 =
    UpdateQueryGenerator.genMacro(tr, "test").update


  val count : Query0[Option[Long]] =
    sql"select count(1) from test".query[Option[Long]]




}

import BenchmarkTestValuesGenerator._
class BenchmarkTestValuesGenerator  extends Specification with IOLiteChecker {

  val transactor = DriverManagerTransactor[IOLite](
    "org.hsqldb.jdbc.JDBCDriver", "jdbc:hsqldb:mem:world", "sa", ""
  )


  (drop.run *> dropSeq.run *>
    create.run *> createSeq.run).transact(transactor).unsafePerformIO

  val List(testedValue) = BenchmarkTestValuesGenerator.generate(1)

  check(count)

}