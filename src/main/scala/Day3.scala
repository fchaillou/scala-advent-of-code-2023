import cats.effect.{IO, IOApp, Resource}

import scala.io.Source

case class NumberWithCoordinates(
    value: Int,
    line: Int,
    startColumn: Int,
    endColumn: Int
) {
  private def rowBefore = (line - 1).max(0)

  def isAdjacent(coordinates: (Int, Int)): Boolean = {
    val (row, col) = coordinates
    val isProperRow = rowBefore <= row && row <= line +1
    val isProperColumn = startColumn - 1 <= col && col <= endColumn + 1
    isProperRow && isProperColumn
  }

  def isEnginePart(engineSchematics: Vector[Vector[Element]]): Boolean = {
    val rows = rowBefore to (line + 1).min(engineSchematics.length - 1)
    val columns = (startColumn - 1).max(0) to (endColumn + 1).min(engineSchematics.head.length - 1)
    rows.exists(row => columns.exists(col => engineSchematics(row)(col) == Element.Symbol || engineSchematics(row)(col) == Element.Gear))
  }
}


enum Element {
  case Digit(value: Int)
  case Gear
  case Symbol
  case Dot
}

object Day3 extends IOApp.Simple {
  override def run: IO[Unit] = for {
    lines <- readInput("day3-input.txt")
    engineSchematics = readEngineSchematics(lines.toVector)
    numbers = engineSchematics.zipWithIndex.flatMap((elements, line) => parseNumbersWithCoordinates(line, elements))
    engineParts = numbers.filter(_.isEnginePart(engineSchematics))
    ignoredParts = numbers.diff(engineParts)
    sum = engineParts.map(_.value).sum
    _ <- IO.println(s"Part1 is $sum")
    gears = findAllGearsCoordinates(engineSchematics)
    adjacentGears = getGearSymbols(gears, engineParts)
    gearsTotal = adjacentGears.map((g1, g2) => g1.value * g2.value).sum
    _ <- IO.println(s"Part2 is $gearsTotal")
  } yield ()

  private def readEngineSchematics(lines: Vector[String]): Vector[Vector[Element]] =
    lines.map(l => l.toVector.map{
      case c if c.isDigit => Element.Digit(c.asDigit)
      case '.' => Element.Dot
      case '*' => Element.Gear
      case _ => Element.Symbol
    })

  private def parseNumbersWithCoordinates(line : Int, schematicsLine: Vector[Element]): Vector[NumberWithCoordinates] = {
    val (numbers, _) = (schematicsLine :+ Element.Dot).zipWithIndex.foldLeft(Vector.empty[NumberWithCoordinates] -> Option.empty[NumberWithCoordinates]) {
      case ((acc, cur), (element, index)) =>
        (element, cur) match {
          case (Element.Digit(value), None) =>
            acc -> Some(NumberWithCoordinates(value, line, index, index))
          case (Element.Digit(value), Some(number)) =>
            val updatedNumber = number.copy(value = number.value * 10 + value, endColumn = index)
            acc -> Some(updatedNumber)
          case (_, Some(number)) => (acc :+ number) -> None
          case (_, _) => acc -> None
        }
    }
    numbers
  }

  private def findAllGearsCoordinates(engineSchematics: Vector[Vector[Element]]): Vector[(Int, Int)] =
    engineSchematics.zipWithIndex.flatMap((row, rowIndex) =>
      row.zipWithIndex.flatMap((e, colIndex) => Option.when(e == Element.Gear)(rowIndex -> colIndex))
    )

  private def getGearSymbols(gears: Vector[(Int, Int)], engineParts: Vector[NumberWithCoordinates]): Vector[(NumberWithCoordinates, NumberWithCoordinates)] = {
    val allPartsNextToGear = gears.map(gear => engineParts.filter(_.isAdjacent(gear)))
    val adjascentGears = allPartsNextToGear.collect {
      case adjacents if adjacents.length == 2 => adjacents.head -> adjacents.tail.head
    }
    adjascentGears
  }
}
