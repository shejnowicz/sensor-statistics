package assignment

import assignment.Model.{Report, SensorAggrData}
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import org.scalacheck.Prop
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap

class ReportTest extends CatsSuite {
  checkAll("PartialResult.SemigroupLaws", SemigroupTests[Report].semigroup)

  test("PartialResult.updateWith_NaN") {
    check(Prop.forAll(notEmptyPartialResult(1)) { (s1: Report) =>
      s1.updateWith(Double.NaN, s1.aggrData.head._1) == Report(s1.aggrData, s1.failedMeasurementsCount + 1, s1.allMeasurementsCount, s1.errors)
    })
  }

  test("PartialResult.updateWith_not_NaN") {
    check(Prop.forAll(notEmptyPartialResult(1),  arbitrary[Double]) { (s1: Report, value: Double) =>
      val aggrDataId = s1.aggrData.head._1
      val aggrData = s1.aggrData.head._2
      s1.updateWith(value, aggrDataId) ==
        Report(
          SortedMap(aggrDataId -> SensorAggrData(Math.min(aggrData.min, value.toInt), Math.max(aggrData.max, value.toInt), aggrData.sum + value.toInt, aggrData.successNo + 1)),
          s1.failedMeasurementsCount,
          s1.allMeasurementsCount + 1,
          s1.errors
        )
    })
  }
}