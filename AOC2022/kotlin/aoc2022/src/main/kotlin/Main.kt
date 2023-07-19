import java.io.File
import java.nio.file.Path
import kotlin.io.path.readLines
import kotlin.time.ExperimentalTime
import kotlin.time.TimeSource

@OptIn(ExperimentalTime::class)
fun main() {


    val timeSource = TimeSource.Monotonic
    val mark1 = timeSource.markNow()

    val path = "C:\\Users\\peter\\Repos\\LearningFSharp\\AOC2022\\kotlin\\aoc2022\\src\\main\\kotlin\\input10.txt"
    val lines = kotlin.io.path.Path(path).readLines()
    //val lines = File(path).readLines()

    val mark2 = timeSource.markNow()

    val elapsed = mark2 - mark1

    println("${elapsed.inWholeMilliseconds}ms")

}


