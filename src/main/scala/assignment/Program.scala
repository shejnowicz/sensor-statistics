package assignment

import java.nio.file.{Path, Paths}

import assignment.Model.Report
import cats.effect.{Concurrent, ExitCode, Sync}
import cats.implicits._
import simulacrum.typeclass
import scala.language.higherKinds

object Program {

  sealed trait PrintEffect
  case class PrintReport(report: Report) extends PrintEffect
  case class PrintException(exception: Throwable) extends PrintEffect

  @typeclass trait Input[F[_]] {
    def data(filepath: Path): fs2.Stream[F, String]
    def fileList(folderPath: Path): F[Iterator[Path]]
  }

  @typeclass trait Output[F[_]] {
    def print(effect: PrintEffect): F[Unit]
  }

  def run[F[_]: Input: Output: Sync: Concurrent](args: List[String]): F[ExitCode] =
    (for {
      fp <- folderPath(args)
      inputFilesIterator <- Input[F].fileList(fp)
      result <- createFinalReport(inputFilesIterator)
      r <- Output[F].print(PrintReport(result))
    } yield r).attempt.flatMap {
      case Left(e) => Output[F].print(PrintException(e)).as(ExitCode.Error)
      case Right(_) => Sync[F].delay(ExitCode.Success)
    }

  private def folderPath[F[_]: Sync](args: List[String]): F[Path] =
    args match {
      case head :: _ => Sync[F].delay(Paths.get(head))
      case Nil => Sync[F].raiseError(new RuntimeException("Folder path not provided"))
    }

  private[Program] def createFinalReport[F[_]: Input: Output: Concurrent](fileListIterator: Iterator[Path]): F[Report] =
    fs2.Stream.fromIterator[F, Path](fileListIterator)
      .mapAsync(10)(dataFilepath => createGroupLeaderReport(dataFilepath))
      .compile
      .foldMonoid

  private[Program] def createGroupLeaderReport[F[_]: Input: Sync](file: Path): F[Report] =
    Input[F].data(file)
      .drop(1)
      .filter(!_.isEmpty)
      .map(_.split(','))
      .map(i => Right((i.head.trim, i.last.trim.toDouble)))
      .handleErrorWith(e => fs2.Stream(Left(e.getMessage)))
      .compile
      .fold(Report.empty) {
        case (a, Right((sId, m))) => a.updateWith(m, sId)
        case (a, Left(e)) => a.updateWithError(e)
      }
}
