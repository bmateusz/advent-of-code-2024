import scala.collection.mutable
import scala.io.Source

@main
def day06(filename: String): Unit =
  // Read the map from the file and convert to a mutable 2D char array
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val grid = lines.map(_.toArray)
  val rows = grid.length
  val cols = grid(0).length

  // Locate guard and initial direction
  var guardRow = 0
  var guardCol = 0
  var direction: Char = '^'
  var found = false
  for (r <- 0 until rows if !found; c <- 0 until cols if !found) {
    val ch = grid(r)(c)
    if ("^v<>".contains(ch)) {
      guardRow = r
      guardCol = c
      direction = ch
      found = true
    }
  }

  // Replace the guard's initial position with '.' to simulate free space
  grid(guardRow)(guardCol) = '.'

  // Define directions in clockwise order to facilitate turning right
  val directions = Array('^', '>', 'v', '<')

  def directionDelta(dir: Char): (Int, Int) = dir match {
    case '^' => (-1, 0)
    case 'v' => (1, 0)
    case '<' => (0, -1)
    case '>' => (0, 1)
  }

  def turnRight(dir: Char): Char = {
    val idx = directions.indexOf(dir)
    directions((idx + 1) % directions.length)
  }

  def isBlocked(r: Int, c: Int, dir: Char): Boolean = {
    val (dr, dc) = directionDelta(dir)
    val nr = r + dr
    val nc = c + dc
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) true
    else grid(nr)(nc) == '#'
  }

  def nextCell(r: Int, c: Int, dir: Char): (Boolean, Int, Int) = {
    val (dr, dc) = directionDelta(dir)
    val nr = r + dr
    val nc = c + dc
    // If next move is out-of-bounds, guard leaves the map
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) (false, nr, nc)
    else if (grid(nr)(nc) == '#') (true, nr, nc) // blocked by #
    else (false, nr, nc) // free space
  }

  val visited = scala.collection.mutable.Set[(Int, Int)]()
  visited.add((guardRow, guardCol))

  var done = false
  while (!done) {
    val (blocked, nr, nc) = nextCell(guardRow, guardCol, direction)
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
      // The guard moves out of the map
      done = true
    } else if (blocked) {
      // Turn right if blocked by #
      direction = turnRight(direction)
    } else {
      // Move forward
      guardRow = nr
      guardCol = nc
      visited.add((guardRow, guardCol))
    }
  }

  println(s"Visited distinct positions: ${visited.size}")

@main
def day06Part2(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val rows = lines.length
  val cols = lines(0).length
  val grid = lines.map(_.toArray)

  // Locate guard's initial position and direction
  var guardRow = 0
  var guardCol = 0
  var direction: Char = '^'
  var found = false
  for (r <- 0 until rows if !found; c <- 0 until cols if !found) {
    val ch = grid(r)(c)
    if ("^v<>".contains(ch)) {
      guardRow = r
      guardCol = c
      direction = ch
      found = true
    }
  }

  // Replace guard's starting position with '.' so it becomes free space after extraction
  grid(guardRow)(guardCol) = '.'

  // Directions in clockwise order: Up, Right, Down, Left
  val directions = Array('^', '>', 'v', '<')

  def directionDelta(dir: Char): (Int, Int) = dir match {
    case '^' => (-1, 0)
    case 'v' => (1, 0)
    case '<' => (0, -1)
    case '>' => (0, 1)
  }

  def turnRight(dir: Char): Char =
    val idx = directions.indexOf(dir)
    directions((idx + 1) % directions.length)

  // Simulate guard movement with an optional extra obstacle
  // Returns true if guard is stuck in a loop, false if guard leaves the map
  def simulate(extraRow: Int, extraCol: Int): Boolean =
    // Copy state
    var r = guardRow
    var c = guardCol
    var d = direction
    val visitedStates = mutable.Set[(Int, Int, Char)]()

    def nextBlocked(r: Int, c: Int, d: Char): Boolean =
      val (dr, dc) = directionDelta(d)
      val nr = r + dr
      val nc = c + dc
      // If next step is out-of-bounds, guard leaves the map => not blocked
      // We'll handle out-of-bounds check after calling this.
      if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) false
      else {
        // Check if next cell is blocked by # or by the placed obstacle
        val cell = if (nr == extraRow && nc == extraCol) '#' else grid(nr)(nc)
        cell == '#'
      }

    while (true) {
      // Detect loop:
      val state = (r, c, d)
      if (visitedStates.contains(state)) {
        // Loop detected
        return true
      }
      visitedStates.add(state)

      val (dr, dc) = directionDelta(d)
      val nr = r + dr
      val nc = c + dc

      // Check if guard would leave the map
      if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
        // Guard leaves the map
        return false
      }

      // Check if blocked
      val blocked = {
        if (nr == extraRow && nc == extraCol && '#' == '#') true
        else grid(nr)(nc) == '#'
      }

      if (blocked) {
        // Turn right if blocked
        d = turnRight(d)
      } else {
        // Move forward
        r = nr
        c = nc
      }
    }
    // Should never reach here
    false

  val possiblePositions = mutable.ListBuffer[(Int, Int)]()

  // Try placing an obstruction in every '.' cell except the guard's initial position
  for (r <- 0 until rows; c <- 0 until cols) {
    if ((r != guardRow || c != guardCol) && grid(r)(c) == '.') {
      if (simulate(r, c)) {
        possiblePositions.append((r, c))
      }
    }
  }

  // Print how many positions lead to a loop
  println(s"Number of positions causing loop: ${possiblePositions.size}")