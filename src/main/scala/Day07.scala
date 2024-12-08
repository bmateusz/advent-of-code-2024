import scala.io.Source

@main
def day07(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Function to evaluate a sequence of numbers with a given sequence of operators (left-to-right)
  def evaluate(numbers: List[Long], ops: List[Char]): Long =
    // There will be one fewer operator than numbers
    var result = numbers.head
    for (i <- ops.indices) {
      val op = ops(i)
      val nextNum = numbers(i + 1)
      op match {
        case '+' => result = result + nextNum
        case '*' => result = result * nextNum
      }
    }
    result

  // Generate all operator combinations
  def generateOperators(count: Long): List[List[Char]] =
    if (count == 0) List(Nil)
    else {
      val smaller = generateOperators(count - 1)
      smaller.map('+' :: _) ++ smaller.map('*' :: _)
    }

  // Check if there's any operator combination that makes numbers produce target
  def canMatchTarget(target: Long, numbers: List[Long]): Boolean =
    if (numbers.size == 1) {
      numbers.head == target
    } else {
      val allOps = generateOperators(numbers.size - 1)
      allOps.exists(ops => evaluate(numbers, ops) == target)
    }

  // Parse each line and check feasibility
  val solvableValues = for {
    line <- lines
    if line.trim.nonEmpty
  } yield {
    val Array(tvPart, numsPart) = line.split(":").map(_.trim)
    val target = tvPart.toLong
    val numbers = numsPart.split("\\s+").map(_.toLong).toList
    if (canMatchTarget(target, numbers)) Some(target) else None
  }

  val total = solvableValues.flatten.sum
  println(total)

import scala.io.Source

@main
def day07Part2(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Evaluate a sequence of numbers with a given sequence of operators left-to-right
  def evaluate(numbers: List[Long], ops: List[Char]): Long =
    var result = numbers.head
    for (i <- ops.indices) {
      val op = ops(i)
      val nextNum = numbers(i + 1)
      op match {
        case '+' =>
          result = result + nextNum
        case '*' =>
          result = result * nextNum
        case '|' =>
          // Concatenate as strings and parse back to Long
          val concatenated = (result.toString + nextNum.toString).toLong
          result = concatenated
      }
    }
    result

  // Generate all operator combinations for (count) positions
  // Operators: '+', '*', '|'
  def generateOperators(count: Int): List[List[Char]] =
    if (count == 0) List(Nil)
    else {
      val smaller = generateOperators(count - 1)
      smaller.flatMap(ops => List('+' :: ops, '*' :: ops, '|' :: ops))
    }

  // Check if there's any operator combination that produces target
  def canMatchTarget(target: Long, numbers: List[Long]): Boolean =
    if (numbers.size == 1) {
      numbers.head == target
    } else {
      val allOps = generateOperators(numbers.size - 1)
      allOps.exists(ops => evaluate(numbers, ops) == target)
    }

  val solvableValues = for {
    line <- lines
    if line.trim.nonEmpty
  } yield {
    val Array(tvPart, numsPart) = line.split(":").map(_.trim)
    val target = tvPart.toLong
    val numbers = numsPart.split("\\s+").map(_.toLong).toList
    if (canMatchTarget(target, numbers)) Some(target) else None
  }

  val total = solvableValues.flatten.sum
  println(total)