import scala.io.Source
import scala.util.matching.Regex

@main
def day03(filename: String): Unit =
  // Read input file
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Combine all lines into a single string
  val input = lines.mkString

  // Regex to match valid mul(X,Y) instructions
  val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r

  // Parse valid instructions and calculate the sum of results
  val result = mulPattern
    .findAllMatchIn(input) // Find all matches of the pattern
    .map { m =>
      val x = m.group(1).toInt // Extract first number
      val y = m.group(2).toInt // Extract second number
      x * y                   // Multiply the numbers
    }
    .sum // Sum up all the results

  // Output the result
  println(s"Sum of results: $result")


import scala.io.Source
import scala.util.matching.Regex

@main
def day03Part2(filename: String): Unit =
  // Read input file
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Combine all lines into a single string
  val input = lines.mkString

  // Regex patterns
  val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r
  val doPattern: Regex = """do\(\)""".r
  val dontPattern: Regex = """don't\(\)""".r

  // State to track whether mul instructions are enabled
  var mulEnabled = true

  // Use a regex to extract all valid patterns (do(), don't(), mul(...))
  val instructionPattern = """(do\(\)|don't\(\)|mul\(\d+,\d+\))""".r

  // Parse instructions and process them
  val result = instructionPattern
    .findAllIn(input)
    .foldLeft(0) { (sum, instruction) =>
      instruction match {
        case "do()" =>
          mulEnabled = true
          sum
        case "don't()" =>
          mulEnabled = false
          sum
        case mulPattern(x, y) if mulEnabled =>
          sum + x.toInt * y.toInt
        case _ =>
          sum
      }
    }

  // Output the result
  println(s"Sum of results: $result")