import scala.io.Source

@main
def day09(filename: String): Unit =
  val source = Source.fromFile(filename)
  val input = source.getLines().mkString
  source.close()

  val disk = parseDisk(input)
  println("Initial Disk Layout:")
  println(disk.mkString)

  val compactedDisk = compactDisk(disk)
  println("Compacted Disk Layout:")
  println(compactedDisk.mkString)

  val checksum = calculateChecksum(compactedDisk)
  println(s"Checksum: $checksum")

def parseDisk(input: String): Array[Char] =
  var disk = Array.empty[Char]
  var fileId = 0
  var isFile = true
  for (ch <- input) {
    val length = ch.asDigit
    if (isFile) {
      disk ++= Array.fill(length)((fileId + '0').toChar)
      fileId += 1
    } else {
      disk ++= Array.fill(length)('.')
    }
    isFile = !isFile
  }
  disk

def compactDisk(disk: Array[Char]): Array[Char] =
  val compacted = disk.clone()

  // Keep moving blocks until no gaps remain before file blocks
  while (true) {
    // Find leftmost '.' and rightmost file block
    val leftmostDot = compacted.indexOf('.')
    if (leftmostDot == -1) {
      // No free spaces at all
      return compacted
    }

    // If there's a '.' but no file block to the right of it, then we are done
    val rightmostBlock = (compacted.lastIndexWhere(_ != '.'))
    if (rightmostBlock == -1 || rightmostBlock < leftmostDot) {
      // No file blocks to the right of a gap means no moves possible
      return compacted
    }

    // Move block from rightmostBlock to leftmostDot
    compacted(leftmostDot) = compacted(rightmostBlock)
    compacted(rightmostBlock) = '.'

    // Print the disk state after each move for debugging
    println("Disk after moving one block:")
    println(compacted.mkString)
  }

  compacted // unreachable, but needed for syntax

def calculateChecksum(disk: Array[Char]): Long =
  disk.zipWithIndex.collect {
    case (block, pos) if block != '.' => (block - '0').toLong * pos
  }.sum


import scala.io.Source

@main
def day09Part2(filename: String): Unit =
  val source = Source.fromFile(filename)
  val input = source.getLines().mkString
  source.close()

  val (disk, files) = parseDiskWithFiles(input)
  println("Initial Disk Layout:")
  println(disk.mkString)

  val compactedDisk = compactDisk(disk, files)
  println("Compacted Disk Layout:")
  println(compactedDisk.mkString)

  val checksum = calculateChecksum(compactedDisk)
  println(s"Checksum: $checksum")

// Parse the input disk map, also record file info: (fileId -> (startIndex, length))
def parseDiskWithFiles(input: String): (Array[Char], Array[(Int, Int, Int)]) =
  var disk = Array.empty[Char]
  var fileId = 0
  var isFile = true

  // We'll record files as we create them
  val filesBuilder = scala.collection.mutable.ArrayBuffer[(Int, Int, Int)]()
  var currentIndex = 0

  for (ch <- input) {
    val length = ch.asDigit
    if (isFile) {
      if (length > 0) {
        val start = currentIndex
        disk ++= Array.fill(length)((fileId + '0').toChar)
        filesBuilder.append((fileId, start, length))
        fileId += 1
        currentIndex += length
      }
    } else {
      // free space
      disk ++= Array.fill(length)('.')
      currentIndex += length
    }
    isFile = !isFile
  }

  (disk, filesBuilder.toArray)

// Compact the disk by moving whole files if possible
def compactDisk(disk: Array[Char], files: Array[(Int, Int, Int)]): Array[Char] =
  val compacted = disk.clone()

  // Sort files by descending fileId
  val filesDesc = files.sortBy(-_._1)

  for ((fid, _, length) <- filesDesc) {
    // Find the current positions of this file on the disk
    val filePositions = compacted.indices.filter(i => compacted(i) == (fid + '0').toChar)
    if (filePositions.nonEmpty) {
      val fileStart = filePositions.min
      val fileEnd = filePositions.max

      // We look to the left of fileStart for a free contiguous span >= length
      // Search from 0 to fileStart-1 for free space
      // We'll find all '.' runs and see if any can fit the file
      val freeSpans = findFreeSpans(compacted, 0, fileStart - 1)

      // Find a free span large enough
      val suitableSpan = freeSpans.find { case (start, spanLen) => spanLen >= length }

      suitableSpan match {
        case Some((spanStart, spanLen)) =>
          // Move the entire file there
          // Clear old position
          filePositions.foreach(pos => compacted(pos) = '.')
          // Place file at spanStart..spanStart+length-1
          for (i <- 0 until length) {
            compacted(spanStart + i) = (fid + '0').toChar
          }
        case None =>
        // No move
      }
    }
  }

  compacted

// Find contiguous free space spans in [0..endIndex], returns list of (start, length)
def findFreeSpans(disk: Array[Char], startIndex: Int, endIndex: Int): List[(Int, Int)] =
  if (endIndex < startIndex) return Nil

  val spans = scala.collection.mutable.ListBuffer[(Int, Int)]()
  var i = startIndex
  while (i <= endIndex) {
    if (disk(i) == '.') {
      val start = i
      while (i <= endIndex && disk(i) == '.') i += 1
      val length = i - start
      spans.append((start, length))
    } else {
      i += 1
    }
  }
  spans.toList
