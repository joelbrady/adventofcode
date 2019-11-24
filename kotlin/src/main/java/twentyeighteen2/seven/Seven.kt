package twentyeighteen2.seven

import assertThat
import java.io.File

fun main() {
    tests()
    example()
    solve()
}

fun solve() {
    File("7.input")
        .bufferedReader()
        .useLines { lines ->
            val input = lines.map { parse(it) }.toList()
            val answer = solve(input)
            println("Part A: $answer")
            val answerB = solveB(input, 5, 60)
            println("PArt B: $answerB")
        }
}

fun tests() {
    assertThat(parse("Step A must be finished before step D can begin."))
        .isEqualTo(Relationship("A", "D"))
}

data class Relationship(val first: String, val then: String)

fun parse(s: String): Relationship {
    val regex = "Step (\\w+) must be finished before step (\\w+) can begin.".toRegex()
    val groups = regex
        .matchEntire(s)
        ?.groups
        ?: throw IllegalStateException()

    return Relationship(groups[1]!!.value, groups[2]!!.value)
}

fun example() {
    val input =
"""Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."""
    .split("\n")

    assertThat(solve(input.map { parse(it) })).isEqualTo("CABDFE")
    assertThat(solveB(input.map { parse(it) }, 2, 0)).isEqualTo(15)
}

fun solve(input: List<Relationship>): String {
    val allNodes: Set<String> = input
        .flatMap { listOf(it.first, it.then) }
        .toSet()

    val prerequisites: Map<String, Set<String>> = buildPrerequisites(input)
    val visited: MutableSet<String> = LinkedHashSet()
    while (visited != allNodes) {
        val requirementsMet = allNodes
            .minus(visited)
            .filter { prerequisites.getOrDefault(it, setOf()).minus(visited).isEmpty() }
            .sorted()
        visited.add(requirementsMet.first())
    }
    return visited.joinToString("")
}

fun solveB(input: List<Relationship>, numWorkers: Int, baseSeconds: Int): Int {
    val allNodes: Set<String> = input
        .flatMap { listOf(it.first, it.then) }
        .toSet()

    val prerequisites: Map<String, Set<String>> = buildPrerequisites(input)
    val visited: MutableSet<String> = LinkedHashSet()
    var secondsElapsed = -1
    val workers: List<Worker> = List(numWorkers) { Worker(baseSeconds) }
    while (visited != allNodes) {
        val busyWorkers = workers.filter { it.busy() }
        busyWorkers
            .forEach {
                val finished = it.tick()
                if (finished != null) {
                    visited.add(finished)
                }
            }
        val freeWorkers = workers.filter { !it.busy() }
        val requirementsMet = allNodes
            .minus(visited)
            .filter { prerequisites.getOrDefault(it, setOf()).minus(visited).isEmpty() }
            .filter { !workedOn(workers, it) }
            .sorted()

        freeWorkers.zip(requirementsMet)
            .forEach { (worker, node) -> worker.workOn(node) }

        secondsElapsed++
    }
    return secondsElapsed
}

fun workedOn(workers: List<Worker>, node: String): Boolean {
    return workers.any { it.workingOn(node) }
}

class Worker(private val baseSeconds: Int) {
    private var node: String? = null
    private var timeLeft: Int = -1
    private val letterCost: Map<String, Int> = ('A'..'Z').map { it.toString() }.zip(1..26).toMap()

    fun workOn(node: String) {
        this.node = node
        timeLeft = baseSeconds + (letterCost[node] ?: throw IllegalStateException())
    }

    fun tick(): String? {
        timeLeft--
        if (timeLeft == 0) {
            val completedWork = node
            node = null
            return completedWork
        }
        return null
    }

    fun busy(): Boolean = timeLeft > 0

    fun workingOn(node: String): Boolean = node == this.node
}

fun buildPrerequisites(input: List<Relationship>): Map<String, Set<String>> {
    return input
        .groupBy { it.then }
        .mapValues { it.value.map { r -> r.first } }
        .mapValues { it.value.toSet() }
}
