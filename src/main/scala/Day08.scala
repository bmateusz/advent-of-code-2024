import scala.io.Source
import scala.collection.mutable
import scala.math.abs

@main
def day08(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val rows = lines.length
  val cols = lines(0).length

  val antennas = mutable.Map[Char, mutable.ListBuffer[(Int, Int)]]()

  for (r <- 0 until rows; c <- 0 until cols) {
    val ch = lines(r)(c)
    if (ch.isLetterOrDigit) {
      // If 'ch' key doesn't exist, create and store a new ListBuffer in the map
      val list = antennas.getOrElseUpdate(ch, mutable.ListBuffer())
      list.append((r, c))
    }
  }

  // A set to hold unique antinode positions
  val antinodes = mutable.Set[(Int, Int)]()

  // For each frequency, consider all pairs of antennas
  for ((freq, positions) <- antennas) {
    val list = positions.toList
    // Generate all pairs
    for (i <- list.indices; j <- i+1 until list.length) {
      val (r1, c1) = list(i)
      val (r2, c2) = list(j)

      // Compute the two antinodes:
      // Antinode1 = 2*A - B = (2*r1 - r2, 2*c1 - c2)
      val antinode1 = (2*r1 - r2, 2*c1 - c2)
      // Antinode2 = 2*B - A = (2*r2 - r1, 2*c2 - c1)
      val antinode2 = (2*r2 - r1, 2*c2 - c1)

      // Check if they are within the map
      if (antinode1._1 >= 0 && antinode1._1 < rows && antinode1._2 >= 0 && antinode1._2 < cols) {
        antinodes.add(antinode1)
      }

      if (antinode2._1 >= 0 && antinode2._1 < rows && antinode2._2 >= 0 && antinode2._2 < cols) {
        antinodes.add(antinode2)
      }
    }
  }

  // Print the count of unique antinodes inside the map
  println(antinodes.size)

@main
def day08Part2(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val rows = lines.length
  val cols = lines(0).length

  val antennas = mutable.Map[Char, mutable.ListBuffer[(Int, Int)]]()

  // Parse the grid, find antennas
  for (r <- 0 until rows; c <- 0 until cols) {
    val ch = lines(r)(c)
    if (ch.isLetterOrDigit) {
      if (!antennas.contains(ch)) {
        antennas(ch) = mutable.ListBuffer()
      }
      antennas(ch).append((r, c))
    }
  }

  val antinodes = mutable.Set[(Int, Int)]()

  // Helper function to compute gcd
  def gcd(a: Int, b: Int): Int =
    if (b == 0) abs(a) else gcd(b, a % b)

  // For each frequency, for each pair of antennas, find all points on line within the map
  for ((freq, positions) <- antennas) {
    val list = positions.toList
    if (list.size >= 2) {
      for (i <- list.indices; j <- i+1 until list.length) {
        val (x1, y1) = list(i)
        val (x2, y2) = list(j)

        val dx = x2 - x1
        val dy = y2 - y1

        // Reduce to step
        val g = gcd(dx, dy)
        val stepX = dx / g
        val stepY = dy / g

        // We have a parametric line: X = x1 + t*stepX, Y = y1 + t*stepY
        // Determine the range of t that keeps the point inside [0, rows) x [0, cols)
        // If stepX = 0, line is vertical, handle separately. If stepY = 0, line is horizontal, handle separately.

        def tRangeForAxis(pos: Int, step: Int, limit: Int): (Double, Double) = {
          // Returns (tMin, tMax) for this axis
          if (step == 0) {
            // No movement along this axis, pos must be in range
            if (pos < 0 || pos >= limit) {
              // No valid t if it's out of range
              (Double.PositiveInfinity, Double.NegativeInfinity)
            } else {
              // Any t is fine for this axis, since position doesn't change.
              (Double.NegativeInfinity, Double.PositiveInfinity)
            }
          } else if (step > 0) {
            // 0 <= pos + t*step < limit
            // -pos/step <= t < (limit-1 - pos)/step
            val tMin = (-pos.toDouble) / step
            val tMax = ((limit - 1 - pos).toDouble) / step
            (tMin, tMax)
          } else {
            // step < 0
            // 0 <= pos + t*step < limit
            //  ( -pos/step ) >= t > ( (limit-1 - pos)/step ) because step<0 reverses inequalities
            // Let's rewrite:
            // pos + t*step >= 0  => t >= -pos/step (since step<0, inequality flips)
            // pos + t*step < limit => t < (limit-1-pos)/step (again step<0 flips inequality)
            val tMin = ((limit - 1 - pos).toDouble) / step // step<0
            val tMax = (-pos.toDouble) / step
            (tMin, tMax)
          }
        }

        val (xMin, xMax) = tRangeForAxis(x1, stepX, rows)
        val (yMin, yMax) = tRangeForAxis(y1, stepY, cols)

        val globalMin = math.max(xMin, yMin)
        val globalMax = math.min(xMax, yMax)

        // We need integer t such that globalMin <= t <= globalMax
        // If globalMin > globalMax, no solutions
        if (globalMin <= globalMax) {
          val startT = math.ceil(globalMin).toInt
          val endT = math.floor(globalMax).toInt

          for (t <- startT to endT) {
            val X = x1 + t * stepX
            val Y = y1 + t * stepY
            if (X >= 0 && X < rows && Y >= 0 && Y < cols) {
              antinodes.add((X, Y))
            }
          }
        }
      }
    }
  }

  // Also, single antennas that appear at frequencies where there is only one antenna do not produce lines.
  // The problem states that "unless that antenna is the only one of its frequency" -
  // If there's only one antenna of that frequency, no line can be formed and no additional antinode is created.
  // If there are multiple antennas of the same frequency, those antennas are also on a line (the line formed by any other pair),
  // and thus are included already in the antinodes set. If you want to ensure antennas are counted when they share a frequency:
  for ((freq, positions) <- antennas) {
    if (positions.size > 1) {
      // Each antenna is on a line that includes at least one other antenna of same frequency
      positions.foreach(antinodes.add)
    }
  }

  println(antinodes.size)
