import assignment.Model.{Report, SensorAggrData}
import cats.Eq
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap

package object assignment {

  implicit val sensorAggrData: Arbitrary[SensorAggrData] =
    Arbitrary(
      for {
        min <- Gen.choose(0, 100)
        max <- Gen.choose(0, 100)
        sum <- Gen.choose(0, 10000)
        successNo <- Gen.choose(0,1000)
      } yield SensorAggrData(min, max, sum, successNo)
    )

  implicit val eqSensorAggrData: Eq[SensorAggrData] = Eq.fromUniversalEquals

  implicit val eqPartialResult: Eq[Report] = Eq.fromUniversalEquals

  implicit val mpartialResult: Arbitrary[Report] =  Arbitrary(partialResult(Gen.listOf(sensorAggrData.arbitrary)))

  def notEmptyPartialResult(size: Int): Gen[Report] = partialResult(Gen.listOfN(size, sensorAggrData.arbitrary))

  private def partialResult(sensorAggrDataGen: Gen[List[SensorAggrData]]) =
    for {
      successMeasurementsCount <- Gen.choose(0, 100)
      failedMeasurementsCount <- Gen.choose(0, 100)
      sessionAggrDataValues <- sensorAggrDataGen
      sessionAggrDataKeys <- Gen.containerOfN[List, String](sessionAggrDataValues.size, arbitrary[Int].map(a => a.toString))
      errors <- Gen.listOf(arbitrary[String])
      fileCount <- arbitrary[Int]
    } yield Report(SortedMap(sessionAggrDataKeys.zip(sessionAggrDataValues):_*), failedMeasurementsCount, successMeasurementsCount, errors, fileCount)
}
