import scala.io.Source

object Day01

@main
def day01(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  val (col1, col2) = lines.map { line =>
    val tokens = line.trim.split("\\s+")
    (tokens(0).toInt, tokens(1).toInt)
  }.unzip

  val result = col1.sorted.zip(col2.sorted).map { (a, b) =>
    Math.abs(a - b)
  }

  println(result.sum)

  println(col1.map(n => n * col2.count(_ == n)).sum)
