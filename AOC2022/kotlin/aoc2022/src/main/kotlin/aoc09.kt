import kotlin.math.abs

data class Coord(val x: Int, val y: Int)

fun aoc09() {
    val path = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/AOC2022/09/input.txt"

    val moves = buildList {
        java.io.File(path).readLines().forEach {
            val s = it.split(" ")
            val dir = s[0]
            val steps = s[1].toInt()
            repeat(steps) {
                add(dir)
            }
        }
    }

    fun moveRight(c: Coord) = c.copy(x = c.x + 1)
    fun moveLeft(c: Coord) = c.copy(x = c.x - 1)
    fun moveUp(c: Coord) = c.copy(y = c.y - 1)
    fun moveDown(c: Coord) = c.copy(y = c.y + 1)

    fun catchUpMove(head: Coord, tail: Coord): Coord {
        val (hx, hy) = head
        val (tx, ty) = tail
        val xd = hx - tx
        val yd = hy - ty

        if (abs(xd) + abs(yd) < 3) return when {
            xd > 1 -> moveRight(tail)
            xd < -1 -> moveLeft(tail)
            yd < -1 -> moveUp(tail)
            yd > 1 -> moveDown(tail)
            else -> tail
        }

        val moveVertical = if (yd < 0) ::moveUp else ::moveDown
        val moveHorizontal = if (xd > 0) ::moveRight else ::moveLeft
        return moveHorizontal(moveVertical(tail))
    }

    buildList {
        val rope = Array(10) { Coord(0, 0) }

        for (move in moves) {
            when (move) {
                "R" -> rope[0] = moveRight(rope[0])
                "L" -> rope[0] = moveLeft(rope[0])
                "U" -> rope[0] = moveUp(rope[0])
                "D" -> rope[0] = moveDown(rope[0])
            }

            for (i in 0 until rope.size - 1) {
                rope[i + 1] = catchUpMove(rope[i], rope[i + 1])
            }

            add(rope[9])
        }
    }.distinct().count().let { println("$it") }

    println()
}