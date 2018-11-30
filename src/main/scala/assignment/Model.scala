package assignment

import cats.{Monoid, Show}
import cats.implicits._
import cats.kernel.Semigroup
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

object Model {
  implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

  type SensorId = String

  case class Report(aggrData: SortedMap[SensorId, SensorAggrData], failedMeasurementsCount: Int, allMeasurementsCount: Int, errors: List[String], fileCount: Int = 1) {

    def updateWith(m: Double, sId: SensorId): Report = {
      Report(
        aggrData + (sId -> aggrData.getOrElse(sId, SensorAggrData.empty).updateWith(m)),
        if(m.isNaN) failedMeasurementsCount + 1 else failedMeasurementsCount,
        allMeasurementsCount + 1,
        errors
      )
    }

    def updateWithError(e: String): Report = Report(aggrData, failedMeasurementsCount, allMeasurementsCount, e :: errors)

    def mergeWith(r: Report) =
      Report(
        Semigroup[SortedMap[String, SensorAggrData]].combine(r.aggrData, aggrData),
        r.failedMeasurementsCount + failedMeasurementsCount,
        r.allMeasurementsCount + allMeasurementsCount,
        r.errors ::: errors,
        r.fileCount + fileCount
      )
  }

  object Report {
    def empty = Report(SortedMap[SensorId, SensorAggrData](), 0, 0, List.empty, 0)

    implicit val groupLeaderReportMonoid: Monoid[Report] = new Monoid[Report] {
      override def empty: Report = Report.empty
      override def combine(x: Report, y: Report): Report = x.mergeWith(y)
    }

    implicit val showResult: Show[Report] = Show.show(r => {
      import r._
      s"""
         |Num of processed files: $fileCount
         |Num of processed measurements: $allMeasurementsCount
         |Num of failed measurements: $failedMeasurementsCount
         |
         |Sensors with highest avg humidity:
         |
         |sensor-id,min,avg,max
         |${aggrData.toList.map { case (sId, agg) =>
              if (agg.successNo > 0)
                s"$sId,${agg.min},${agg.avg},${agg.max}"
              else
                s"$sId,NaN,NaN,NaN"
            }.mkString("\n")
          }
        """.stripMargin
    })
  }

  case class SensorAggrData(min: Int, max: Int, sum: Int, successNo: Int) {
    def avg: Int = if (!isEmpty) sum / successNo else 0

    def updateWith(m: Double): SensorAggrData =
      if(m.isNaN)
        this
      else
        this.copy(
          if (m < min) m.toInt else min,
          if (m > max) m.toInt else max,
          sum + m.toInt,
          successNo + 1
        )


    def mergeWith(aggr: SensorAggrData) =
      SensorAggrData(Math.min(min, aggr.min), Math.max(max, aggr.max), sum + aggr.sum, successNo + aggr.successNo)

    def isEmpty: Boolean = successNo == 0
  }

  object SensorAggrData {
    def empty = SensorAggrData(Int.MaxValue, Int.MinValue, 0, 0)

    implicit val sensorAggrDataSemigroup: Semigroup[SensorAggrData] = (x: SensorAggrData, y: SensorAggrData) => x.mergeWith(y)

    implicit val sensorAggrDataOrdering = new Ordering[SensorAggrData] {
      override def compare(x: SensorAggrData, y: SensorAggrData): Int = if(x.avg > y.avg) 1 else if(x.avg < y.avg) -1 else 0
    }
  }

}
