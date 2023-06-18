import java.util.PriorityQueue

fun main() {
    val graph = HashMap<String, List<Pair<String, Int>>>()
    val distances = hashMapOf<String, Int>()
    val previous = HashMap<String, String?>()

    val source = "a"
    graph["a"] = listOf("c" to 5, "b" to 2)
    graph["b"] = listOf("a" to 2, "c" to 3, "d" to 3)
    graph["c"] = listOf("a" to 5, "b" to 3, "e" to 2)
    graph["d"] = listOf("a" to 7, "b" to 3, "e" to 1)
    graph["e"] = listOf("c" to 2, "d" to 1)

    val queue = PriorityQueue<Pair<String, Int>>(compareBy { it.second })
    val infinite = 1000

    for (v in graph.keys) {
        distances[v] = infinite
        previous[v] = null
        queue.add(v to infinite)
    }
    distances[source] = 0

    while (queue.isNotEmpty()) {
        val (edge, _) = queue.poll()

        graph[edge]?.forEach { (neighbor, weight) ->
            val totalDistance = distances.getValue(edge) + weight
            if (totalDistance < distances.getValue(neighbor)) {
                distances[neighbor] = totalDistance
                previous[neighbor] = edge
                queue.add(neighbor to totalDistance)
            }
        }
    }

    assert(previous["e"] == "d")
    assert(previous["d"] == "b")
    assert(previous["b"] == "a")

    println("Done")
}


