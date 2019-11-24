import java.io.File

const val FILENAME = "1.input"

fun main() {
    File(FILENAME)
        .bufferedReader()
        .useLines { lines ->
            lines
                .toList()
                .map { Integer.parseInt(it) }
                .let { ns -> sum(ns).also { findFirstRepeatedState(ns) } }
        }
}

fun sum(lines: List<Int>) {
    println("Answer to 1A: ${lines.sum()}")
}

fun findFirstRepeatedState(ns: List<Int>) {
    val seen: MutableSet<Int> = mutableSetOf()
    var i = 0
    var currentFrequency = 0
    while (!seen.contains(currentFrequency)) {
        seen.add(currentFrequency)
        val delta = ns[i % ns.size]
        currentFrequency += delta
        i++
    }

    println("Answer to 1B: $currentFrequency")
}
