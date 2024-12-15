import scala.io.Source
import scala.collection.mutable
import scala.util.chaining._
import scala.util.Try
import scala.math.Ordering.Implicits.infixOrderingOps

@main
def day12(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val rows = lines.length
  val cols = if rows == 0 then 0 else lines(0).length
  val grid = Array.ofDim[Char](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols) {
    grid(r)(c) = lines(r)(c)
  }

  // To track visited cells
  val visited = Array.ofDim[Boolean](rows, cols)

  def inBounds(r: Int, c: Int) = r >= 0 && r < rows && c >= 0 && c < cols

  val directions = Array((1, 0), (-1, 0), (0, 1), (0, -1))

  def bfs(startR: Int, startC: Int): (Int, Int) =
    // Returns (area, perimeter) of the region
    val plantType = grid(startR)(startC)
    val queue = mutable.Queue((startR, startC))
    visited(startR)(startC) = true

    var area = 0
    var perimeter = 0

    while (queue.nonEmpty) {
      val (r, c) = queue.dequeue()
      area += 1
      // Check neighbors for perimeter calculation
      for ((dr, dc) <- directions) {
        val nr = r + dr
        val nc = c + dc
        if (!inBounds(nr, nc)) {
          // Out of bounds contributes to perimeter
          perimeter += 1
        } else {
          if (grid(nr)(nc) != plantType) {
            // Different type contributes to perimeter
            perimeter += 1
          } else if (!visited(nr)(nc)) {
            // Same type, not visited => part of the region
            visited(nr)(nc) = true
            queue.enqueue((nr, nc))
          }
        }
      }
    }

    (area, perimeter)

  var totalPrice = 0L

  // Find all regions
  for (r <- 0 until rows; c <- 0 until cols) {
    if (!visited(r)(c)) {
      val (area, perimeter) = bfs(r, c)
      val price = area.toLong * perimeter.toLong
      totalPrice += price
    }
  }

  println(totalPrice)

// Directions: up, right, down, left
val Dirs: List[(Int, Int)] = List((-1, 0), (0, 1), (1, 0), (0, -1))

case class Region(cellType: Char, cells: List[(Int, Int)]) {
  def area: Int = cells.size
}

@main
def day12Part2(filename: String): Unit = {
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()
  println(s"Input:\n${lines.mkString("\n")}")

  val rows = lines.length
  val cols = if rows == 0 then 0 else lines(0).length
  val grid = Array.ofDim[Char](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols) {
    grid(r)(c) = lines(r)(c)
  }

  // Find all regions
  val visited = Array.fill(rows, cols)(false)
  val regions = mutable.Buffer[Region]()

  for (r <- 0 until rows; c <- 0 until cols) {
    if (!visited(r)(c)) {
      val region = floodFill(grid, visited, r, c)
      regions += region
    }
  }

  val intersections = findIntersections(grid)
  println(s"Intersections: $intersections")

  var totalPrice = 0
  for ((reg, idx) <- regions.zipWithIndex) {
    val sides = countSides(grid, intersections, reg)
    val price = reg.area * sides
    println(s"Region $idx of type '${reg.cellType}': area=${reg.area}, sides=$sides, price=$price")
    // Debug printing cells:
    println(s"  Cells: ${reg.cells}")
    totalPrice += price
  }

  println(s"Total price: $totalPrice")
}

def floodFill(grid: Array[Array[Char]], visited: Array[Array[Boolean]], sr: Int, sc: Int): Region = {
  val rows = grid.length
  val cols = grid(0).length
  val startType = grid(sr)(sc)
  val q = mutable.Queue[(Int, Int)]()
  val cells = mutable.ListBuffer[(Int, Int)]()
  q.enqueue((sr, sc))
  visited(sr)(sc) = true

  while (q.nonEmpty) {
    val (r, c) = q.dequeue()
    cells += ((r, c))
    for ((dr, dc) <- Dirs) {
      val nr = r + dr
      val nc = c + dc
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        if (!visited(nr)(nc) && grid(nr)(nc) == startType) {
          visited(nr)(nc) = true
          q.enqueue((nr, nc))
        }
      }
    }
  }

  Region(startType, cells.toList)
}

def findIntersections(grid: Array[Array[Char]]): mutable.Set[(Int, Int)] = {
  val rows = grid.length
  val cols = grid(0).length
  val intersections = mutable.Set[(Int, Int)]()

  for (r <- 0 until rows - 1; c <- 0 until cols - 1) {
    // Check the 2x2 block of cells
    val topLeft = grid(r)(c)
    val topRight = grid(r)(c + 1)
    val bottomLeft = grid(r + 1)(c)
    val bottomRight = grid(r + 1)(c + 1)

    // Check for "AB / BA" intersection pattern
    if (topLeft != topRight && bottomLeft != bottomRight && topLeft == bottomRight && bottomLeft == topRight) {
      intersections += ((r + 1, c + 1))
    }
  }

  intersections
}

def countSides(grid: Array[Array[Char]], intersections: mutable.Set[(Int, Int)], region: Region): Int = {
  val rows = grid.length
  val cols = grid(0).length
  val cellSet = region.cells.toSet

  val edges = mutable.Set[((Int, Int), (Int, Int))]()

  def addEdge(p1: (Int, Int), p2: (Int, Int)): Unit = {
    import scala.math.Ordering.Implicits.infixOrderingOps
    val edge = if (p1 < p2) (p1, p2) else (p2, p1)
    edges += edge
  }

  // Identify boundary edges
  for ((r, c) <- region.cells) {
    if (r == 0 || !cellSet.contains((r - 1, c))) {
      addEdge((r, c), (r, c + 1)) // top
    }
    if (r == rows - 1 || !cellSet.contains((r + 1, c))) {
      addEdge((r + 1, c), (r + 1, c + 1)) // bottom
    }
    if (c == 0 || !cellSet.contains((r, c - 1))) {
      addEdge((r, c), (r + 1, c)) // left
    }
    if (c == cols - 1 || !cellSet.contains((r, c + 1))) {
      addEdge((r, c + 1), (r + 1, c + 1)) // right
    }
  }

  def isHorizontal(e: ((Int, Int), (Int, Int))): Boolean = {
    val ((r1, c1), (r2, c2)) = e
    r1 == r2
  }

  def isVertical(e: ((Int, Int), (Int, Int))): Boolean = {
    val ((r1, c1), (r2, c2)) = e
    c1 == c2
  }

  var sides = 0

  while (edges.nonEmpty) {
    val edge = edges.min
    println(s"Side ${sides + 1} starts from $edge")
    edges.remove(edge)

    // Determine direction
    val ((r1, c1), (r2, c2)) = edge
    if (r1 == r2) {
      // Horizontal run
      val row = r1
      // Start from this segment and move forward along the row
      var startCol = c1
      var endCol = c2

      // Go right
      var nextEdgeRight = ((row, endCol), (row, endCol + 1))
      while (!intersections.contains(nextEdgeRight._1) && edges.remove(nextEdgeRight)) {
        endCol += 1
        nextEdgeRight = ((row, endCol), (row, endCol + 1))
      }

      // Go left
      var nextEdgeLeft = ((row, startCol - 1), (row, startCol))
      while (!intersections.contains(nextEdgeLeft._2) && edges.remove(nextEdgeLeft)) {
        startCol -= 1
        nextEdgeLeft = ((row, startCol - 1), (row, startCol))
      }

      // Count one side for this horizontal run
      sides += 1
    } else {
      // Vertical run
      val col = c1
      var startRow = r1
      var endRow = r2

      // Go downward
      var nextEdgeDown = ((endRow, col), (endRow + 1, col))
      while (!intersections.contains(nextEdgeDown._1) && edges.remove(nextEdgeDown)) {
        endRow += 1
        nextEdgeDown = ((endRow, col), (endRow + 1, col))
      }

      // Go upward
      var nextEdgeUp = ((startRow - 1, col), (startRow, col))
      while (!intersections.contains(nextEdgeUp._2) && edges.remove(nextEdgeUp)) {
        startRow -= 1
        nextEdgeUp = ((startRow - 1, col), (startRow, col))
      }

      // Count one side for this vertical run
      sides += 1
    }
  }

  sides
}

@main
def day12Part2Test(): Unit =
  // must be 936718 for input
  for (i <- 1 to 5) {
    day12Part2(s"input/day12/example$i.txt")
  }

@main
def day12Part2Attempt2(filename: String): Unit = {
  // Read input
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val rows = lines.length
  val cols = if rows == 0 then 0 else lines(0).length
  val grid = Array.ofDim[Char](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols) {
    grid(r)(c) = lines(r)(c)
  }

  val visited = Array.ofDim[Boolean](rows, cols)

  def inBounds(r: Int, c: Int) = r >= 0 && r < rows && c >= 0 && c < cols

  // dR = [1,0,-1,0], dC = [0,1,0,-1]
  // This means:
  // i=0: Up
  // i=1: Right
  // i=2: Down
  // i=3: Left
  val dRow = Array(1, 0, -1, 0)
  val dCol = Array(0, 1, 0, -1)

  def sides(row: Int, col: Int): Int = {
    val plant = grid(row)(col)
    var plantSides = 0
    for (i <- 0 until 4) {
      val newR = row + dRow(i)
      val newC = col + dCol(i)
      val boundaryOrDifferent = !inBounds(newR, newC) || grid(newR)(newC) != plant

      if (boundaryOrDifferent) {
        val newRow90CC = row + dRow((i - 1 + 4) % 4)
        val newCol90CC = col + dCol((i - 1 + 4) % 4)
        val isBeginEdge = !inBounds(newRow90CC, newCol90CC) || grid(newRow90CC)(newCol90CC) != plant

        val newRowCorner = newR + dRow((i - 1 + 4) % 4)
        val newColCorner = newC + dCol((i - 1 + 4) % 4)
        val isConcaveBeginEdge = inBounds(newRowCorner, newColCorner) && grid(newRowCorner)(newColCorner) == plant

        if (isBeginEdge || isConcaveBeginEdge) {
          plantSides += 1
        }
      }
    }
    plantSides
  }

  def priceRegion(startRow: Int, startCol: Int): Long = {
    val plant = grid(startRow)(startCol)
    val queue = mutable.Queue((startRow, startCol))
    visited(startRow)(startCol) = true

    var area = 1L
    var region_sides = sides(startRow, startCol)

    while (queue.nonEmpty) {
      val (row, col) = queue.dequeue()
      for (i <- 0 until 4) {
        val newR = row + dRow(i)
        val newC = col + dCol(i)
        if (inBounds(newR, newC) && !visited(newR)(newC) && grid(newR)(newC) == plant) {
          visited(newR)(newC) = true
          area += 1
          region_sides += sides(newR, newC)
          queue.enqueue((newR, newC))
        }
      }
    }

    area * region_sides
  }

  var totalPrice = 0L
  for (r <- 0 until rows; c <- 0 until cols) {
    if (!visited(r)(c)) {
      totalPrice += priceRegion(r, c)
    }
  }

  println(s"Part 2 answer: $totalPrice")
}