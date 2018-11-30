package assignment

import java.nio.file.{Files, Path}

import assignment.Program._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Stream, io, text}

import scala.concurrent.ExecutionContext.global
import scala.collection.JavaConverters._

object BootIO extends IOApp {

  implicit val ioInput: Input[IO] = new Input[IO]{
    override def data(filepath: Path): Stream[IO, String] = io.file.readAll[IO](filepath, global, 4096).through(text.utf8Decode).through(text.lines)
    override def fileList(folderPath: Path): IO[Iterator[Path]] = IO(Files.list(folderPath).iterator().asScala)
  }

  implicit val ioOutput: Output[IO] = {
    case PrintReport(report) => IO(println(report.show))
    case PrintException(e) => IO(e.printStackTrace())
  }

  def run(args: List[String]): IO[ExitCode] = Program.run[IO](args)
}
