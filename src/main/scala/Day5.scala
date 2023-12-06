import cats.syntax.all.*
import cats.effect.{IO, IOApp}

import scala.collection.immutable.NumericRange

object Day5 extends IOApp.Simple {
  override def run: IO[Unit] = for {
    lines <- readInput("day5-input.txt")
    seeds = lines.head.drop(7).split(' ').map(_.toLong)
    gardenMapping = IslandGardenMapping.parse(lines.drop(2).toVector)
    seedsLocation = seeds.map(gardenMapping.getSeedLocation)
    lowestLocation = seedsLocation.min
    _ <- IO.println(s"Part 1 Lowest location is $lowestLocation")
    seedRanges = seeds.grouped(2).toList.map(arrayToRange).zipWithIndex
    lowestLocationPerSeedGroup <- seedRanges.parTraverse((range, groupIndex) => computeMinForRange(gardenMapping)(groupIndex, range))
    lowestLocationPart2 = lowestLocationPerSeedGroup.min
    _ <- IO.println(s"Part 2 Lowest location is $lowestLocationPart2")
  } yield ()

  private def arrayToRange(list: Array[Long]): NumericRange.Inclusive[Long] = {
    val start = list.head
    val size = list.tail.head
    start to start + size - 1
  }

  private def computeMinForRange(gardenMapping: IslandGardenMapping)(group: Int, range: NumericRange.Inclusive[Long]): IO[Long] =
   for {
     _ <- IO.println(s"Computing min for group $group of ${range.size} elements")
     min <- IO(range.foldLeft(Long.MaxValue)((min, seed) => gardenMapping.getSeedLocation(seed).min(min)))
     _ <- IO.println(s"Found $min for group $group")
   } yield min

}

case class SparseMap(
    keys: Vector[NumericRange.Inclusive[Long]],
    values: Vector[NumericRange.Inclusive[Long]]
) {

  def get(key: Long): Long = {
    val index = keys.indexWhere(_.contains(key))
    if (index == -1) key
    else {
      val keyRange = keys(index)
      val valueRange = values(index)
      key - keyRange.start + valueRange.start
    }
  }
}

object SparseMap {
  def parse(lines: Vector[String]): SparseMap = {
    val (sourceRanges, destinationRanges) = lines.map(parseLine).unzip
    SparseMap(sourceRanges, destinationRanges)
  }

  private def parseLine(line: String): (NumericRange.Inclusive[Long], NumericRange.Inclusive[Long]) = {
    line.split(' ').toList match {
      case destinationRangeStartStr :: sourceRangeStartStr :: rangeLengthStr :: Nil =>
        val sourceRangeStart = sourceRangeStartStr.toLong
        val destinationRangeStart = destinationRangeStartStr.toLong
        val rangeLength = rangeLengthStr.toLong
        (sourceRangeStart to sourceRangeStart + rangeLength) -> (destinationRangeStart to destinationRangeStart + rangeLength)
      case _ => sys.error("Invalid number of elements")
    }
  }
}

case class IslandGardenMapping(
    seedToSoil: SparseMap,
    soilToFertilizer: SparseMap,
    fertilizerToWater: SparseMap,
    waterToLight: SparseMap,
    lightToTemperature: SparseMap,
    temperatureToHumidity: SparseMap,
    humidityToLocation: SparseMap
) {
  private val seedToLocation: Long => Long = List(
    seedToSoil,
    soilToFertilizer,
    fertilizerToWater,
    waterToLight,
    lightToTemperature,
    temperatureToHumidity,
    humidityToLocation
  ).map(_.get).foldLeft[Long => Long](identity)(_ andThen _)

  def getSeedLocation(seed: Long): Long = seedToLocation(seed)
}

enum Section(val name: String) {
  case SeedToSoil extends Section("seed-to-soil")
  case SoilToFertilizer extends Section("soil-to-fertilizer ")
  case FertilizerToWater extends Section("fertilizer-to-water")
  case WaterToLight extends Section("water-to-light")
  case LightToTemperature extends Section("light-to-temperature")
  case TemperatureToHumidity extends Section("temperature-to-humidity")
  case HumidityToLocation extends Section("humidity-to-location")
}

object IslandGardenMapping {
  def parse(input: Vector[String]): IslandGardenMapping = {
    val sectionMap = splitInputInSections(input)
    IslandGardenMapping(
      seedToSoil = sectionMap(Section.SeedToSoil),
      soilToFertilizer = sectionMap(Section.SoilToFertilizer),
      fertilizerToWater = sectionMap(Section.FertilizerToWater),
      waterToLight = sectionMap(Section.WaterToLight),
      lightToTemperature = sectionMap(Section.LightToTemperature),
      temperatureToHumidity = sectionMap(Section.TemperatureToHumidity),
      humidityToLocation = sectionMap(Section.HumidityToLocation)
    )
  }


  private def splitInputInSections(input: Vector[String]): Map[Section, SparseMap] = {
    def sectionTitleToSection(title: String): Section =
      Section.values.find(s => title.startsWith(s.name))
        .getOrElse(sys.error(s"No section found for $title"))

    LazyList.unfold(input) {
      case v if v.isEmpty => None
      case remainingInput =>
        val (section, rest) = remainingInput.span(line => !line.isBlank)
        val sectionName = sectionTitleToSection(section.head)
        val sectionMapping = sectionName -> SparseMap.parse(section.tail)

        val restWithoutEmptyLine = if(rest.isEmpty) rest else rest.tail
        Some(sectionMapping -> restWithoutEmptyLine)
    }.toMap
  }
}

