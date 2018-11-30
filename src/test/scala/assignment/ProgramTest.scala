package assignment

import java.io.IOException
import java.nio.file.{Path, Paths}

import assignment.Model.{Report, SensorAggrData}
import assignment.Program._
import cats.data.{Chain, WriterT}
import cats.effect.{ContextShift, ExitCode, IO}
import fs2.Stream
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.GivenWhenThen

import scala.collection.immutable.SortedMap

class ProgramTest extends WordSpec with GivenWhenThen with Matchers{

  type WIO[E] = WriterT[IO, Chain[PrintEffect], E]
  object WIO {
    def apply[E](v: E): WIO[E] = WriterT(IO(Chain(), v))
    def apply[E](v: E, se: PrintEffect): WIO[E] = WriterT(IO(Chain(se), v))
  }

  implicit val ioContextShift: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  implicit val ioOutputMock: Output[WIO] = (e: PrintEffect) => WIO({}, e)

  "Measurements" should {

    "be calculated successfully when data is spread over multiple files" in {
      Given("Two files with sensor data: /1 and /2")
      val testFolderPath = Paths.get("/folderPath")
      val file1 = Stream("sensor-id,humidity", "s1,10", "s2,88", "s1,NaN")
      val file2 = Stream("sensor-id,humidity", "s2,80", "s3,NaN", "s2,78", "s1,98")

      implicit val ioInputMock: Input[WIO] = new Input[WIO]{
        override def data(filepath: Path): Stream[WIO, String] = if (filepath == Paths.get("/1")) file1 else if (filepath == Paths.get("/2")) file2 else Stream.empty
        override def fileList(folderPath: Path): WIO[Iterator[Path]] = WIO(if(testFolderPath == folderPath) Iterator(Paths.get("/1"), Paths.get("/2")) else Iterator.empty)
      }

      When("Program is run")
      val (sideEffects, res) = Program.run[WIO](List(testFolderPath.toString)).run.unsafeRunSync()

      Then("Aggregations are successfully calculated")
      res should equal(ExitCode.Success)
      sideEffects.size should equal(1)
      sideEffects.toList.head should equal(
        PrintReport(
          Report(
            SortedMap(
              "s2" -> SensorAggrData(78, 88, 246, 3),
              "s1" -> SensorAggrData(10, 98, 108, 2),
              "s3" -> SensorAggrData.empty
            ), 2, 7, List.empty, 2
          )
        )
      )
    }

    "not be calculated when folder doesn't exists" in {
      Given("Not existing folder")
      val testFolderPath = Paths.get("/folderPath")
      val folderNotFoundException = new IOException()
      implicit val ioInputMock: Input[WIO] = new Input[WIO]{
        override def data(filepath: Path): Stream[WIO, String] = Stream.empty
        override def fileList(folderPath: Path): WIO[Iterator[Path]] = WIO({throw folderNotFoundException})
      }

      When("Program is run")
      val (sideEffects, res) = Program.run[WIO](List(testFolderPath.toString)).run.unsafeRunSync()

      Then("Program finished with folderNotFoundException")
      res should equal(ExitCode.Error)
      sideEffects.size should equal(1)
      sideEffects.toList.head should equal(PrintException(folderNotFoundException))
    }
  }
}
