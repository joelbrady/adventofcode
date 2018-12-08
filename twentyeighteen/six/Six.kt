package twentyeighteen.six

import assertThat
import java.io.File

// idea is to:
// build the grid
// calculate the closest points for each spot
// then go around the edges to find the infinite ones
// then iterate over the non-infinite ones and calculate their area
// sort and pick the biggest unique

fun main() {
    tests()

    example()

    problem()
}

fun tests() {
    val here = Coordinates(10, 10)
    val there = Coordinates(13, 8)
    assertThat(here.distanceTo(there)).isEqualTo(5)
}

fun example() {
    val input =
"""1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""
    .split("\n")

    val answer = solve(input)

    assertThat(answer).isEqualTo(17)

    val answerB = solveB(input, 32)
    assertThat(answerB).isEqualTo(16)
}


fun problem() {
    File("6.input")
        .bufferedReader()
        .useLines {
            val input = it.toList()
            val answer = solve(input)
            println("Part A: $answer")
            val answerB = solveB(input, 10000)
            println("Part B: $answerB")
        }
}

fun solveB(input: List<String>, maxDistance: Int): Int {
    val points = parseInput(input)
    val buffer = maxDistance + 2
    val minRow = -buffer
    val maxRow = points
        .map { it.row }
        .max() ?: throw IllegalStateException(".")
        + buffer
    val minCol = -buffer
    val maxCol = points
        .map { it.col }
        .max() ?: throw IllegalStateException(".")
        + buffer

    var inRange = 0
    for (row in minRow..maxRow) {
        for (col in minCol..maxCol) {
            val here = Coordinates(col, row)
            val distances = points
                .map { here.distanceTo(it) }
            if (distances.sum() < maxDistance) {
                inRange++
            }
        }
    }
    return inRange
}

fun solve(input: List<String>): Int {
    // need a set for ties
    val grid: MutableMap<Coordinates, Set<Int>> = mutableMapOf()
    val points = parseInput(input)
        .zip(1..input.size)
        .map { Point(it.second, it.first) }
        .groupBy { it.coords }
        .mapValues { it.value.first() }

    val maxRow = points
        .keys
        .map { it.row }
        .max() ?: throw IllegalStateException("no entries")
    val maxCol = points
        .keys
        .map { it.col }
        .max() ?: throw IllegalStateException("no entries")

    // find closest node for each place within the bounded grid
    for (row in 0..maxRow) {
        for (col in 0..maxCol) {
            val here = Coordinates(col, row)
            val distances = points
                .values
                .map { Distance(it.id, here.distanceTo(it.coords)) }
            val minimum = distances.map { it.distance }.min() ?: throw IllegalStateException("no entries?")
            grid[here] = distances
                .filter { it.distance == minimum }
                .map { it.node }
                .toSet()
        }
    }

    val allIds = points
        .values
        .map { it.id }
        .toSet()

    val infiniteAreas: MutableSet<Int> = mutableSetOf()
    for (col in 0..maxCol) {
        val top = Coordinates(col, 0)
        val bottom = Coordinates(col, maxRow)
        val thingsAtTop = grid[top] ?: setOf()
        val thingsAtBottom = grid[bottom] ?: setOf()
        if (thingsAtTop.size == 1) {
            infiniteAreas.addAll(thingsAtTop)
        }
        if (thingsAtBottom.size == 1) {
            infiniteAreas.addAll(thingsAtBottom)
        }
    }
    for (row in 0..maxRow) {
        val left = Coordinates(0, row)
        val right = Coordinates(maxCol, row)
        val thingsOnLeft = grid[left] ?: setOf()
        val thingsOnRight = grid[right] ?: setOf()
        if (thingsOnLeft.size == 1) {
            infiniteAreas.addAll(thingsOnLeft)
        }
        if(thingsOnRight.size == 1) {
            infiniteAreas.addAll(thingsOnRight)
        }
    }
    val boundedAreas = allIds.minus(infiniteAreas)
    val largestBoundedAreaSize = boundedAreas
        .map { numberOfSquaresOnlyCoveredBy(it, grid, maxRow, maxCol) }
        .max()
    return largestBoundedAreaSize ?: throw IllegalStateException("wat")
}

private fun parseInput(input: List<String>) = input
    .map { it.split(", ").let { (col, row) -> Coordinates(col.toInt(), row.toInt()) } }

fun numberOfSquaresOnlyCoveredBy(id: Int, grid: Map<Coordinates, Set<Int>>, maxRow: Int, maxCol: Int): Int {
    var count = 0
    for (row in 0..maxRow) {
        for (col in 0..maxCol) {
            val here = Coordinates(col, row)
            val set = grid[here] ?: setOf()
            if (set.size == 1 && id in set) {
                count++
            }
        }
    }
    return count
}

data class Distance(val node: Int, val distance: Int)
data class Point(val id: Int, val coords: Coordinates)
data class Coordinates(val col: Int, val row: Int) {
    fun distanceTo(there: Coordinates): Int {
        return Math.abs(this.col - there.col) + Math.abs(this.row - there.row)
    }
}
