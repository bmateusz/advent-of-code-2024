import scala.io.Source
import scala.collection.mutable

@main
def day05(filename: String): Unit =
  // Read input file
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Separate rules and updates
  val (ruleLines, updateLines) = lines.span(_.contains("|"))
  val rules = ruleLines.map { line =>
    val Array(before, after) = line.split('|').map(_.toInt)
    (before, after)
  }
  val updates = updateLines.tail.map(_.split(',').map(_.toInt).toList)

  // Function to check if an update respects the rules
  def isValidUpdate(update: List[Int], rules: List[(Int, Int)]): Boolean =
    val positionMap = update.zipWithIndex.toMap // Map page -> position
    rules.forall {
      case (before, after) =>
        positionMap.get(before).forall { posBefore =>
          positionMap.get(after).forall(posAfter => posBefore < posAfter)
        }
    }

  // Find middle page of a list
  def middlePage(update: List[Int]): Int = update(update.length / 2)

  // Validate updates and calculate the sum of middle pages
  val validUpdates = updates.filter(update => isValidUpdate(update, rules))
  val middleSum = validUpdates.map(middlePage).sum

  // Output the result
  println(s"Sum of middle pages: $middleSum")

@main
def day05Part2(filename: String): Unit =
  // Read input file
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  // Separate rules and updates
  val (ruleLines, updateLines) = lines.span(_.contains("|"))
  val rules = ruleLines.map { line =>
    val Array(before, after) = line.split('|').map(_.toInt)
    (before, after)
  }
  val updates = updateLines.tail.map(_.split(',').map(_.toInt).toList)

  // Function to check if an update respects the rules
  def isValidUpdate(update: List[Int], rules: List[(Int, Int)]): Boolean =
    val positionMap = update.zipWithIndex.toMap // Map page -> position
    rules.forall {
      case (before, after) =>
        positionMap.get(before).forall { posBefore =>
          positionMap.get(after).forall(posAfter => posBefore < posAfter)
        }
    }

  // Function to reorder an update using topological sort
  def reorderUpdate(update: List[Int], rules: List[(Int, Int)]): List[Int] =
    val updateSet = update.toSet
    val filteredRules = rules.filter { case (before, after) => updateSet.contains(before) && updateSet.contains(after) }

    // Build adjacency list
    val adjacencyList = mutable.Map[Int, mutable.Set[Int]]().withDefaultValue(mutable.Set())
    val inDegree = mutable.Map[Int, Int]().withDefaultValue(0)

    for (before, after) <- filteredRules do
      adjacencyList(before) += after
      inDegree(after) += 1

    // Add nodes with zero in-degree to the queue
    val zeroInDegree = mutable.Queue[Int]()
    update.foreach { node =>
      if (inDegree(node) == 0) zeroInDegree.enqueue(node)
    }

    // Perform topological sort
    val sortedList = mutable.ListBuffer[Int]()
    while zeroInDegree.nonEmpty do
      val node = zeroInDegree.dequeue()
      sortedList += node
      for neighbor <- adjacencyList(node) do
        inDegree(neighbor) -= 1
        if inDegree(neighbor) == 0 then zeroInDegree.enqueue(neighbor)

    sortedList.toList

  // Find middle page of a list
  def middlePage(update: List[Int]): Int = update(update.length / 2)

  // Split updates into valid and invalid
  val (validUpdates, invalidUpdates) = updates.partition(update => isValidUpdate(update, rules))

  // Reorder invalid updates
  val reorderedInvalidUpdates = invalidUpdates.map(update => reorderUpdate(update, rules))

  // Compute the sum of middle pages of reordered invalid updates
  val middleSum = reorderedInvalidUpdates.map(middlePage).sum

  // Output the result
  println(s"Sum of middle pages after reordering: $middleSum")