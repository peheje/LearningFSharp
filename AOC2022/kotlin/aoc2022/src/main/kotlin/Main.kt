import java.io.File

data class Monkey(
    val number: Int,
    val items: MutableList<Int>,
    val operation: (Int) -> Int,
    val toThrowIndex: (Int) -> Int,
    var inspected: Int = 0
)

fun main() {
    val path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/kotlin/aoc2022/src/main/kotlin/input11.txt"
    val monkeysRaw = File(path).readText().split("\n\n")

    val monkeys = monkeysRaw.map { monkeyRaw ->
        val monkeyRows = monkeyRaw.split("\n").map { it.trim() }
        val monkeyNumber = monkeyRows[0].replace("Monkey ", "").replace(":", "").toInt()
        val startingItems = monkeyRows[1].replace("Starting items: ", "").split(", ").map { it.toInt() }.toMutableList()
        val operation = monkeyRows[2].replace("Operation: new = old ", "").split(" ")

        val op = when {
            operation[1] == "old" -> { a: Int -> a * a }
            operation[0] == "*" -> { a: Int -> a * operation[1].toInt() }
            operation[0] == "+" -> { a: Int -> a + operation[1].toInt() }
            else -> throw Exception("Unhandled operation")
        }
        val testDivisibleBy = monkeyRows[3].replace("Test: divisible by ", "").toInt()
        val ifTrue = monkeyRows[4].replace("If true: throw to monkey ", "").toInt()
        val ifFalse = monkeyRows[5].replace("If false: throw to monkey ", "").toInt()
        val throwToIndex = { a: Int -> if (a % testDivisibleBy == 0) ifTrue else ifFalse }

        Monkey(monkeyNumber, startingItems, op, throwToIndex)
    }

    val roundsTotal = 20
    var round = 0
    while (round < roundsTotal) {
        for (monkey in monkeys) {
            val thrownIndices = mutableSetOf<Int>()
            for (i in 0 until monkey.items.size) {
                monkey.inspected++
                monkey.items[i] = monkey.operation(monkey.items[i]) / 3

                val throwTo = monkey.toThrowIndex(monkey.items[i])
                monkeys[throwTo].items.add(monkey.items[i])
                thrownIndices.add(i)
            }
            for (i in monkey.items.size - 1 downTo 0) {
                if (thrownIndices.contains(i)) {
                    monkey.items.removeAt(i)
                }
            }
        }

        round++
    }

    val mostActive = monkeys.sortedByDescending { it.inspected }.take(2)
    println(mostActive[0].inspected * mostActive[1].inspected)
}
