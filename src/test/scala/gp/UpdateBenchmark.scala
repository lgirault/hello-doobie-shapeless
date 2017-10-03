package gp

import doobie.imports.DriverManagerTransactor
import doobie.util.iolite.IOLite
import org.scalameter.api._
import org.scalameter.{CurveData, Persistor, Reporter, log}
import org.scalameter.utils.Tree
import doobie.imports._
import cats.implicits._
import fs2.interop.cats._


case class MyLoggingReporter[T]() extends Reporter[T] {

  def report(result: CurveData[T], persistor: Persistor) {
    // output context
    log(s"::Benchmark ${result.context.scope}::")
//    val machineKeys = result.context.properties
//      .filterKeys(Context.machine.properties.keySet.contains).toSeq.sortBy(_._1)
//    for ((key, value) <- machineKeys) {
//      log(s"$key: $value")
//    }

    // output measurements
    for (measurement <- result.measurements) {
      log(s" -> ${measurement.value} ms")
      //log(s"${measurement.params}: ${measurement.value}")
    }

    // add a new line
    log("")
  }

  def report(result: Tree[CurveData[T]], persistor: Persistor) = true

}

import BenchmarkTestValuesGenerator._
class UpdateBenchmark
  extends Bench.LocalTime {

  import org.scalameter.picklers.noPickler._

  override lazy val reporter = new MyLoggingReporter[Double]

  val transactor = DriverManagerTransactor[IOLite](
    "org.hsqldb.jdbc.JDBCDriver", "jdbc:hsqldb:mem:world", "sa", ""
  )

  println("generating values ...")
  val testBase = BenchmarkTestValuesGenerator.generate(1000)
  println("values genarated !")

  val resources: Gen[List[TestedResource]] = Gen.single("resources")(testBase)

  (drop.run *> dropSeq.run *>
    create.run *> createSeq.run).transact(transactor).unsafePerformIO

  measure method "insert using macro generated method" in {
    using(resources) in {
      rs => rs map ( r => insertMacro(r).transact(transactor).unsafePerformIO)
    }
  }


  measure method "insert using shapeless generated method" in {
    using(resources) in {
      rs => rs map ( r => insertMacro(r).transact(transactor).unsafePerformIO)
    }
  }



  val updateBase1 = testBase map { r=>
    val t =  insertMacro(r).transact(transactor).unsafePerformIO
    randomWithId(t.i1)
  }

  val updateBase2 = testBase map { r=>
    val t =  insertMacro(r).transact(transactor).unsafePerformIO
    randomWithId(t.i1)
  }

  val updatedResources1: Gen[List[TestedResource]] = Gen.single("resources")(updateBase1)
  val updatedResources2: Gen[List[TestedResource]] = Gen.single("resources")(updateBase2)


  measure method "update using macro generated method" in {
    using(updatedResources1) in {
      rs => rs map ( r => updateMacro(r).run.transact(transactor).unsafePerformIO)
    }
  }


  measure method "update using shapeless generated method" in {
    using(updatedResources2) in {
      rs => rs map ( r => updateShapelessAnnotation(r).run.transact(transactor).unsafePerformIO)
    }
  }

}
