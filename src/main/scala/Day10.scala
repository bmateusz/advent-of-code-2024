import scala.io.Source
import scala.collection.mutable

@main
def day10(filename: String): Unit =
  val lines = Source.fromFile(filename).getLines().toArray
  Source.fromFile(filename).close()

  val rows = lines.length
  val cols = lines(0).length
  val heightMap = Array.ofDim[Int](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols) {
    heightMap(r)(c) = lines(r)(c) - '0'
  }

  // Directions for up/down/left/right
  val directions = Array((1,0),(-1,0),(0,1),(0,-1))

  // Memoization cache: Map[(r, c), Set[(r9, c9)] of reachable 9s]
  // Instead of storing sets of coordinates directly (which could be large),
  // we'll store them in a mutable structure. But we must be careful with memory.
  //
  // Another option: we just store a reference to a cached result. We'll store a Set of positions as a cached result.
  // Each DFS returns a Set of (r,c) where height=9.

  val memo = mutable.Map[(Int, Int), Set[(Int, Int)]]()

  def inBounds(r: Int, c: Int): Boolean = r >= 0 && r < rows && c >= 0 && c < cols

  def dfs(r: Int, c: Int): Set[(Int, Int)] =
    // Check cache
    memo.get((r,c)) match {
      case Some(res) => res
      case None =>
        val h = heightMap(r)(c)
        if (h == 9) {
          // If this cell is height 9, reachable set is just itself
          val res = Set((r,c))
          memo((r,c)) = res
          res
        } else {
          // Look for neighbors with height h+1
          val nextH = h+1
          var result = Set[(Int, Int)]()
          for ((dr, dc) <- directions) {
            val nr = r+dr
            val nc = c+dc
            if (inBounds(nr,nc) && heightMap(nr)(nc) == nextH) {
              val subRes = dfs(nr,nc)
              result = result union subRes
            }
          }
          memo((r,c)) = result
          result
        }
    }

  // Identify trailheads (height=0)
  val trailheads = for {
    r <- 0 until rows
    c <- 0 until cols
    if heightMap(r)(c) == 0
  } yield (r,c)

  var totalScore = 0
  for ((r,c) <- trailheads) {
    val reachable9s = dfs(r,c)
    val score = reachable9s.size
    totalScore += score
  }

  println(totalScore)
import scala.io.Source
import scala.collection.mutable

@main
def day10Part2(filename: String): Unit =
  val lines = Source.fromFile(filename).getLines().toArray
  Source.fromFile(filename).close()

  val rows = lines.length
  val cols = lines(0).length
  val heightMap = Array.ofDim[Int](rows, cols)
  for (r <- 0 until rows; c <- 0 until cols) {
    heightMap(r)(c) = lines(r)(c) - '0'
  }

  // Directions for up/down/left/right
  val directions = Array((1, 0), (-1, 0), (0, 1), (0, -1))

  // Memoization cache: Map[(r, c), Int] -> number of distinct hiking trails
  val memo = mutable.Map[(Int, Int), Int]()

  def inBounds(r: Int, c: Int): Boolean = r >= 0 && r < rows && c >= 0 && c < cols

  // DFS to count distinct hiking trails starting from (r, c) with height `h`
  def countTrails(r: Int, c: Int): Int =
    // Check cache
    memo.get((r, c)) match {
      case Some(res) => res
      case None =>
        val h = heightMap(r)(c)
        if (h == 9) {
          // A trail ending at height 9 is a valid trail
          memo((r, c)) = 1
          1
        } else {
          // Explore neighbors with height h+1
          val nextH = h + 1
          var totalTrails = 0
          for ((dr, dc) <- directions) {
            val nr = r + dr
            val nc = c + dc
            if (inBounds(nr, nc) && heightMap(nr)(nc) == nextH) {
              totalTrails += countTrails(nr, nc)
            }
          }
          memo((r, c)) = totalTrails
          totalTrails
        }
    }

  // Identify trailheads (height=0)
  val trailheads = for {
    r <- 0 until rows
    c <- 0 until cols
    if heightMap(r)(c) == 0
  } yield (r, c)

  var totalRating = 0
  for ((r, c) <- trailheads) {
    val rating = countTrails(r, c)
    totalRating += rating
  }

  println(totalRating)