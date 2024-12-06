import scala.io.Source

@main
def day02(filename: String): Unit =
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  val data: List[List[Int]] = lines.map { line =>
    line.trim.split("\\s+").map(_.toInt).toList
  }

  def isSafe(levels: List[Int]): Boolean = {
    val diffs = levels.zip(levels.tail).map { case (a, b) =>
      val diff = a - b
      if (Math.abs(diff) <= 3)
        if (diff > 0) 1
        else if (diff < 0) -1
        else 0
      else
        diff
    }
    diffs.forall(_ == 1) || diffs.forall(_ == -1)
  }

  val safeReports = data.count { levels =>
    if (isSafe(levels)) true
    else {
      levels.indices.exists { i =>
        val newLevels = levels.take(i) ++ levels.drop(i + 1)
        if (newLevels.length >= 2) isSafe(newLevels) else false
      }
    }
  }
  println(safeReports)