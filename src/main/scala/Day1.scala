import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}

import scala.io.Source

object Day1 extends IOApp.Simple {
  private val lettersAsNumber =
    Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    )

  override def run: IO[Unit] =
    for {
      lines <- readInput("day1-list.txt")
      numbers = lines.map(extractLineNumber)
      result = numbers.sum
      _ <- IO.println(s"Sum of all Calibration values : ${result}")
    } yield ()

  private def extractLineNumber(line: String): Int = {
    def splitStringAtNumbers(line: String): List[String | Int] = {
      line.toCharArray.toVector.foldRight(List.empty[String | Int]) {
        (prevChar, list) =>
          list match {
            case Nil if !prevChar.isDigit => s"$prevChar" :: Nil
            case Nil                      => prevChar.asDigit :: Nil
            case ::(head: String, next) if !prevChar.isDigit =>
              s"$prevChar$head" :: next
            case ::(head: String, next) if prevChar.isDigit =>
              prevChar.asDigit :: head :: next
            case ::(head: Int, next) if prevChar.isDigit =>
              prevChar.asDigit :: head :: next
            case ::(head: Int, next) => prevChar.toString :: head :: next
          }
      }
    }

    val digitsVector = splitStringAtNumbers(line)
      .flatMap {
        case s: String => extractNumberAsString(s)
        case i: Int    => List(i)
      }

    val firstDigit = digitsVector.head
    val lastDigit = digitsVector.last

    firstDigit * 10 + lastDigit
  }

  private def extractNumberAsString(text: String): List[Int] = {
    def substrings(str: String): Iterator[String] =
      if (str.isEmpty) Iterator.empty
      else {
        str.inits ++ substrings(str.tail)
      }

    substrings(text).collect {
      case str if lettersAsNumber.contains(str) => lettersAsNumber(str)
    }.toList
  }
}
