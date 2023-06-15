import java.io.File

data class Monkey(
    val number: Int,
    val items: MutableList<Long>,
    val operation: (Long) -> Long,
    val toThrowIndex: (Long) -> Int,
    var inspected: Long = 0,
    val testDivisibleBy: Long
)

fun main() {
    val path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/kotlin/aoc2022/src/main/kotlin/input11.txt"
    val monkeysRaw = File(path).readText().split("\n\n")

    val monkeys = monkeysRaw.map { monkeyRaw ->
        val rows = monkeyRaw.split("\n").map { it.trim() }
        val monkeyNumber = rows[0].replace("Monkey ", "").replace(":", "").toInt()
        val startingItems = rows[1].replace("Starting items: ", "").split(", ").map { it.toLong() }.toMutableList()
        val operation = rows[2].replace("Operation: new = old ", "").split(" ")

        val op = when {
            operation[1] == "old" -> { a: Long -> a * a }
            operation[0] == "*" -> { a: Long -> a * operation[1].toLong() }
            operation[0] == "+" -> { a: Long -> a + operation[1].toLong() }
            else -> throw Exception("Unhandled operation")
        }
        val testDivisibleBy = rows[3].replace("Test: divisible by ", "").toLong()
        val ifTrue = rows[4].replace("If true: throw to monkey ", "").toInt()
        val ifFalse = rows[5].replace("If false: throw to monkey ", "").toInt()
        val throwToIndex = { a: Long -> if (a % testDivisibleBy == 0L) ifTrue else ifFalse }

        Monkey(monkeyNumber, startingItems, op, throwToIndex, 0, testDivisibleBy)
    }

    val commonDivisor = monkeys.map { it.testDivisibleBy }.reduce { acc, i -> acc * i }

    val roundsTotal = 10000
    var round = 0
    while (round < roundsTotal) {
        for (monkey in monkeys) {
            for (i in 0 until monkey.items.size) {
                monkey.inspected++
                // monkey.items[i] = monkey.operation(monkey.items[i]) / 3  // part 1
                monkey.items[i] = monkey.operation(monkey.items[i]).mod(commonDivisor) // part 2
                val throwTo = monkey.toThrowIndex(monkey.items[i])
                monkeys[throwTo].items.add(monkey.items[i])
            }
            monkey.items.clear()
        }

        round++

        if (round == 1 || round == 20 || round % 1000 == 0) {
            println("== After round $round ==")
            for (monkey in monkeys) {
                println("Monkey ${monkey.number} inspected ${monkey.inspected} times.")
            }
        }
    }

    val mostActive = monkeys
        .map { it.inspected }
        .sortedDescending()
        .take(2)
    println(mostActive[0] * mostActive[1])
}
