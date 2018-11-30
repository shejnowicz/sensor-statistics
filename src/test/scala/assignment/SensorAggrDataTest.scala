package assignment

import assignment.Model.SensorAggrData
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import org.scalacheck.Prop

class SensorAggrDataTest extends CatsSuite {

  checkAll("SensorAggrData.SemigroupLaws", SemigroupTests[SensorAggrData].semigroup)

  test("PartialResult.isEmpty") {
    check(Prop.forAll {(s1: SensorAggrData) => s1.isEmpty == (s1.successNo == 0)})
  }

  test("PartialResult.updateWith_NaN") {
    check(Prop.forAll {(s1: SensorAggrData) => s1.updateWith(Double.NaN) == s1})
  }

  test("PartialResult.updateWith_not_NaN") {
    check(Prop.forAll { (s1: SensorAggrData, value: Double) =>
      s1.updateWith(value) == SensorAggrData(Math.min(s1.min, value.toInt), Math.max(s1.max, value.toInt), s1.sum + value.toInt, s1.successNo + 1)
    })
  }
}
