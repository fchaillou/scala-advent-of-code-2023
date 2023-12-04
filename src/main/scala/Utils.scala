import cats.effect.IO
import cats.effect.kernel.Resource

import scala.io.Source

def readInput(resourceName: String): IO[List[String]] =
  Resource
    .fromAutoCloseable(IO(Source.fromResource(resourceName)))
    .use(src => IO(src.getLines().toList))