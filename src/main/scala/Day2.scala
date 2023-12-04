import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}

import scala.io.Source

object Day2 extends IOApp.Simple {

  override def run: IO[Unit] = for {
    lines <- readInput("day2-input.txt")
    games = lines.map(Game.parse)
    input = Possibility(blue = 14, red = 12, green = 13)
    possibleGames = games.filter(_.isPossible(input))
    sum = possibleGames.map(_.id).sum
    _ <- IO.println(s"Part 1 result is $sum")
    powers = games.map(_.minPossibility.power)
    summedPowers = powers.sum
    _ <- IO.println(s"Part 2 result is $summedPowers")
  } yield ()
}

object Possibility {
  private val regex = " (\\d{1,2}) (red|green|blue)".r

  def parse(in: String): Possibility = in.split(",").foldLeft(Possibility(0, 0, 0)) { (p, str) =>
    str match
      case regex(count, "red") => p.copy(red = count.toInt)
      case regex(count, "green") => p.copy(green = count.toInt)
      case regex(count, "blue") => p.copy(blue = count.toInt)
  }
}

case class Possibility(blue: Int, red: Int, green: Int) {
  def isPossible(input: Possibility): Boolean = {
    blue <= input.blue && red <= input.red && green <= input.green
  }
  def power: Int = blue * red * green
}

case class Game(id: Int, grabs: List[Possibility]) {
  def isPossible(input: Possibility): Boolean = grabs.forall(_.isPossible(input))

  def minPossibility: Possibility =
    Possibility(
      red = grabs.maxBy(_.red).red,
      green = grabs.maxBy(_.green).green,
      blue = grabs.maxBy(_.blue).blue,
    )
}

object Game {
  private val regex = "Game (\\d+):(.*+)".r

  def parse(line: String): Game = line match
    case regex(id, possibilities) => Game(id.toInt, possibilities.split(';').map(Possibility.parse).toList)
}