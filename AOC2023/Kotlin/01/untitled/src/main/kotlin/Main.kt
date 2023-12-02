import java.io.File
import java.lang.Exception

fun main(args: Array<String>) {
    val rows = File("C:\\Users\\peter\\Repos\\LearningFSharp\\AOC2023\\01\\data01").readLines()

    val part1 = rows.sumOf { row ->
        val first = row.find { it.isDigit() }.toString()
        val last = row.findLast { it.isDigit() }.toString()
        (first + last).toInt()
    }

    println(part1)

    val letters = mapOf(
        "one" to 1,
        "two" to 2,
        "three" to 3,
        "four" to 4,
        "five" to 5,
        "six" to 6,
        "seven" to 7,
        "eight" to 8,
        "nine" to 9,
        "0" to 0,
        "1" to 1,
        "2" to 2,
        "3" to 3,
        "4" to 4,
        "5" to 5,
        "6" to 6,
        "7" to 7,
        "8" to 8,
        "9" to 9
    )

    val lettersReversed = letters.mapKeys { it.key.reversed() }

    val part2 = rows.sumOf {
        val first = findFirstDigit(it, letters)
        val last = findFirstDigit(it.reversed(), lettersReversed)
        (first + last).toInt()
    }
    println(part2)
}

fun findFirstDigit(row: String, letters: Map<String, Int>): String {
    var left = row

    while (left.isNotEmpty()) {
        for ((letter, letterValue) in letters) {
            if (left.startsWith(letter)) return letterValue.toString()
        }
        left = left.substring(1)
    }

    throw Exception("Will always find a first digit")
}
