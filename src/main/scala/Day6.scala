import cats.effect.{IO, IOApp}

object Day6 extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      lines <- readInput("day6-input.txt")
      boatRaces = BoatRace.parseAll(lines)
      possibleSolutions = boatRaces.map(_.possibleSolutions)
      resultPart1 = possibleSolutions.foldLeft(1)(_ * _.size)
      _ <- IO.println(s"Part1 solution is $resultPart1")
      boatRace = BoatRace.parseKerned(lines)
      _ <- IO.println(
        s"Part2 solution is ${boatRace.possibleSolutions.size}"
      )
    } yield ()
}

case class BoatRace(duration: Long, maxDistance: Long) {
  def possibleSolutions: LazyList[Long] =
    LazyList
      .unfold(0L) { holdTime =>
        Option.when(holdTime <= duration) {
          val runtime = duration - holdTime
          holdTime * runtime -> (holdTime + 1)
        }
      }
      .filter(_ > maxDistance)

}

object BoatRace {
  def parseAll(lines: List[String]): Vector[BoatRace] = {
    val durations =
      lines.head
        .substring("Time:".length)
        .trim
        .split("\\s")
        .filterNot(_.isBlank)
        .map(_.toLong)
    val maxDistances =
      lines(1)
        .substring("Distance:".length)
        .trim
        .split("\\s")
        .filterNot(_.isBlank)
        .map(_.toLong)

    durations.zip(maxDistances).map(BoatRace.apply.tupled).toVector
  }

  def parseKerned(lines: List[String]): BoatRace = {
    val duration =
      lines.head
        .substring("Time:".length)
        .trim
        .filterNot(_.isSpaceChar)
        .toLong
    val maxDistance =
      lines(1)
        .substring("Distance:".length)
        .trim
        .filterNot(_.isSpaceChar)
        .toLong

    BoatRace(duration, maxDistance)
  }
}
