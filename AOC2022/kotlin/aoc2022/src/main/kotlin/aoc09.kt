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

    fun moveRight(c: Coord) = Coord(c.x + 1, c.y)
    fun moveLeft(c: Coord) = Coord(c.x - 1, c.y)
    fun moveUp(c: Coord) = Coord(c.x, c.y - 1)
    fun moveDown(c: Coord) = Coord(c.x, c.y + 1)

    fun catchUpMove(head: Coord, tail: Coord): Coord {
        val (hx, hy) = head
        val (tx, ty) = tail
        val xd = hx - tx
        val yd = hy - ty

        return if (abs(xd) + abs(yd) < 3) {
            if (xd > 1) moveRight(tail)
            else if (xd < -1) moveLeft(tail)
            else if (yd < -1) moveUp(tail)
            else if (yd > 1) moveDown(tail)
            else tail
        } else {
            val upOrDown = if (yd < 0) ::moveUp else ::moveDown
            val leftOrRight = if (xd > 0) ::moveRight else ::moveLeft
            leftOrRight(upOrDown(tail))
        }
    }

    sequence {
        val rope = MutableList(10) { Coord(0, 0) }

        for (move in moves) {
            val (hx, hy) = rope[0]

            when (move) {
                "R" -> rope[0] = Coord(hx + 1, hy)
                "L" -> rope[0] = Coord(hx - 1, hy)
                "U" -> rope[0] = Coord(hx, hy - 1)
                "D" -> rope[0] = Coord(hx, hy + 1)
            }

            for (i in 0 until rope.size - 1) {
                rope[i + 1] = catchUpMove(rope[i], rope[i + 1])
            }

            yield(rope[9])
        }
    }.distinct().count().let { println("$it") }

    println()
}