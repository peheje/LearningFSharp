import java.io.File

private fun aoc10Part0(): List<Int> {
    val path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/10/input10.txt"
    val cycles = File(path).readLines().map {
        val s = it.split(" ")
        when (s[0]) {
            "noop" -> listOf(0)
            "addx" -> listOf(0, s[1].toInt())
            else -> throw Exception("unhandled operation")
        }
    }.flatten()

    return cycles
}

fun aoc10Part1() {
    val cycles = aoc10Part0()
    var x = 1
    var sum = 0
    cycles.forEachIndexed { i, value ->
        val cycle = i + 1
        if ((cycle + 20) % 40 == 0) {
            println("$cycle: $x")
            sum += (cycle * x)
        }
        x += value
    }

    println(sum)
}

fun aoc10Part2() {

    fun inWindow(centerOfWindow: Int, value: Int): Boolean {
        val center = centerOfWindow % 40
        return value == (center - 1) ||
                value == center ||
                value == (center + 1)
    }

    val cycles = aoc10Part0()

    var x = 1
    val sb = StringBuilder()
    cycles.forEachIndexed { i, value ->
        if (inWindow(centerOfWindow = i, value = x))
            sb.append("#")
        else
            sb.append(".")

        x += value

        if ((i + 1) % 40 == 0)
            sb.append("\n")
    }

    println(sb.toString())
}