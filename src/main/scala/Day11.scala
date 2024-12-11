import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

@main
def day11(filename: String): Unit =
  val source = Source.fromFile(filename)
  val line = source.getLines().next().trim
  source.close()

  // Initial stones from input line (space-separated)
  val stones = ArrayBuffer[String]()
  if (line.nonEmpty) {
    stones ++= line.split("\\s+")
  }

  // Define function to split even-digit number into two stones
  def splitEven(numStr: String): (String, String) =
    val mid = numStr.length / 2
    val leftHalf = numStr.substring(0, mid)
    val rightHalf = numStr.substring(mid)
    val leftStone = trimLeadingZeros(leftHalf)
    val rightStone = trimLeadingZeros(rightHalf)
    (leftStone, rightStone)

  def trimLeadingZeros(s: String): String =
    val res = s.dropWhile(_ == '0')
    if (res.isEmpty) "0" else res

  // Define function to multiply by 2024
  def multiply2024(numStr: String): String =
    // Use BigInt for simplicity
    val num = BigInt(numStr)
    (num * 2024).toString

  // Apply one blink transformation
  def blink(stones: ArrayBuffer[String]): ArrayBuffer[String] =
    val newStones = ArrayBuffer[String]()
    for (stone <- stones) {
      if (stone == "0") {
        // rule 1
        newStones += "1"
      } else {
        val length = stone.length
        if (length % 2 == 0 && length != 0) {
          // rule 2: even number of digits
          val (leftStone, rightStone) = splitEven(stone)
          newStones += leftStone
          newStones += rightStone
        } else {
          // rule 3: multiply by 2024
          // (also covers the case if length=0 which shouldn't happen since stone always has digits)
          val newStone = multiply2024(stone)
          newStones += newStone
        }
      }
    }
    newStones

  var current = stones
  for (blinkCount <- 1 to 75) {
    current = blink(current)
    println(s"After $blinkCount blinks: ${current.size} stones")
  }

@main
def day11Graph(filename: String): Unit =
  val source = Source.fromFile(filename)
  val line = source.getLines().next().trim
  source.close()

  val initialStones = if (line.nonEmpty) line.split("\\s+").toVector else Vector.empty[String]
  val blinks = 75 // or any number of blinks

  // Global memoization for transformations: stone -> next stones after one blink
  val nextMemo = mutable.Map[String, Vector[String]]()

  // Memo for counting results: (stone, n) -> count
  val countMemo = mutable.Map[(String, Int), Long]()

  // Transformations
  def splitEven(numStr: String): (String, String) =
    val mid = numStr.length / 2
    val leftHalf = numStr.substring(0, mid)
    val rightHalf = numStr.substring(mid)
    (trimLeadingZeros(leftHalf), trimLeadingZeros(rightHalf))

  def trimLeadingZeros(s: String): String =
    val res = s.dropWhile(_ == '0')
    if (res.isEmpty) "0" else res

  def multiply2024(numStr: String): String =
    val num = BigInt(numStr)
    (num * 2024).toString

  // Compute next stones for a given stone (one blink)
  def nextBlinkStones(stone: String): Vector[String] =
    nextMemo.getOrElseUpdate(stone, {
      if (stone == "0") {
        // Rule 1: 0 -> 1
        Vector("1")
      } else if (stone.length % 2 == 0 && stone.nonEmpty) {
        // Rule 2: even number of digits -> split
        val (l, r) = splitEven(stone)
        Vector(l, r)
      } else {
        // Rule 3: multiply by 2024
        Vector(multiply2024(stone))
      }
    })

  // Count how many stones result from a single stone after n blinks
  def countStones(stone: String, n: Int): Long =
    if (n == 0) {
      1L
    } else {
      countMemo.getOrElseUpdate((stone, n), {
        val nextSt = nextBlinkStones(stone)
        nextSt.map(s => countStones(s, n - 1)).sum
      })
    }

  // For the entire initial arrangement, sum the counts
  val total = initialStones.map(st => countStones(st, blinks)).sum

  println(s"After $blinks blinks: $total stones")
