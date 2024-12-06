import scala.io.Source

@main
def day04(filename: String): Unit =
  // Read the word search puzzle from the file
  val source = Source.fromFile(filename)
  val grid = source.getLines().toArray.map(_.toCharArray)
  source.close()

  val targetWord = "XMAS"
  val directions = Seq(
    (0, 1),   // Horizontal right
    (1, 0),   // Vertical down
    (1, 1),   // Diagonal down-right
    (1, -1),  // Diagonal down-left
    (0, -1),  // Horizontal left
    (-1, 0),  // Vertical up
    (-1, -1), // Diagonal up-left
    (-1, 1)   // Diagonal up-right
  )

  def countWordOccurrences(grid: Array[Array[Char]], word: String): Int =
    val rows = grid.length
    val cols = grid(0).length
    val wordLength = word.length

    def isWordAt(row: Int, col: Int, dir: (Int, Int)): Boolean =
      val (dr, dc) = dir
      (0 until wordLength).forall { i =>
        val nr = row + i * dr
        val nc = col + i * dc
        nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid(nr)(nc) == word(i)
      }

    // Count occurrences of the word starting from each cell in all directions
    (for
      row <- 0 until rows
      col <- 0 until cols
      dir <- directions
      if isWordAt(row, col, dir)
    yield 1).sum

  // Count occurrences of "XMAS"
  val totalOccurrences = countWordOccurrences(grid, targetWord)
  println(s"Total occurrences of '$targetWord': $totalOccurrences")
import scala.io.Source

@main
def day04Part2(filename: String): Unit =
  // Read the word search puzzle from the file
  val source = Source.fromFile(filename)
  val grid = source.getLines().toArray.map(_.toCharArray)
  source.close()

  val targetMAS = "MAS"
  val reversedMAS = targetMAS.reverse

  def countXMASOccurrences(grid: Array[Array[Char]]): Int =
    val rows = grid.length
    val cols = grid(0).length
    val masLength = targetMAS.length

    // Function to check if a MAS or its reverse is present in a diagonal direction
    def isMASAt(row: Int, col: Int, dr: Int, dc: Int): Boolean =
      val candidates = Seq(targetMAS, reversedMAS)
      candidates.exists { mas =>
        (0 until masLength).forall { i =>
          val nr = row + i * dr
          val nc = col + i * dc
          nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid(nr)(nc) == mas(i)
        }
      }

    // Function to check for an X-MAS pattern centered at a specific cell
    def isXMASAt(centerRow: Int, centerCol: Int): Boolean =
      isMASAt(centerRow - 1, centerCol - 1, 1, 1) && // Top-left to bottom-right MAS
        isMASAt(centerRow - 1, centerCol + 1, 1, -1)   // Top-right to bottom-left MAS

    // Count occurrences of X-MAS patterns
    (for
      row <- 1 until rows - 1
      col <- 1 until cols - 1
      if isXMASAt(row, col) // Check with the center coordinates
    yield 1).sum

  // Count X-MAS occurrences
  val totalOccurrences = countXMASOccurrences(grid)
  println(s"Total occurrences of X-MAS: $totalOccurrences")
