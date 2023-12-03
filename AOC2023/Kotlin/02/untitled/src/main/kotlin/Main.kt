import java.io.File

fun main(args: Array<String>) {
    val redLimit = 12
    val greenLimit = 13
    val blueLimit = 14
    val rows =
        File("C:\\Users\\peter\\Repos\\LearningFSharp\\AOC2023\\Kotlin\\02\\untitled\\src\\main\\kotlin\\example02").readLines()
    val games = rows.map { toGame(it) }

    val part1 = games
        .filter { game ->
            !game.sets.any { set -> set.blue > blueLimit || set.green > greenLimit || set.red > redLimit }
        }.sumOf { it.gameId }

    println(part1)
}

data class Game(val sets: List<Set>, val gameId: Int)
data class Set(val red: Int, val green: Int, val blue: Int)

fun toGame(row: String): Game {
    val numberStartIndex = row.indexOf(" ")
    val startIndex = row.indexOf(":")
    val gameNumber = row.substring(numberStartIndex + 1, startIndex).toInt()
    val game = row.substring(startIndex + 2)
    val rawSets = game.split("; ")
    val sets = rawSets.map { rawSet ->
        val cubes = rawSet.split(", ")
        var red = 0
        var green = 0
        var blue = 0
        cubes.forEach { cube ->
            val cubePlays = cube.split(" ")
            val amount = cubePlays[0].toInt()
            val color = cubePlays[1]
            when (color) {
                "blue" -> blue = amount
                "green" -> green = amount
                "red" -> red = amount
            }
        }
        Set(red, green, blue)
    }
    return Game(sets, gameNumber)
}
