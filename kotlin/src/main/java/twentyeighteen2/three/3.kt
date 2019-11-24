package twentyeighteen2.three

import assertThat
import java.io.File
import java.lang.IllegalArgumentException

fun main() {
    tests()

    File("3.input")
        .bufferedReader()
        .useLines { lines -> solve(lines) }
}

fun solve(input: Sequence<String>) {
    val f = Fabric()

    input
        .map { parse(it) }
        .forEach { f.mark(it) }

    println("Part A: ${f.totalCoveredMoreThanNTimes(1)}")
    println("Part B: ${f.soloClaim()}")
}

fun tests() {
    testFabric()
    testParsing()
    example()
}

fun testFabric() {
    val f = Fabric()
    val c = Claim(1, 3, 2, 2, 5)
    f.mark(c)

    assertThat(f.numberOfClaimsAt(0, 0)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(0, 1)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(1, 1)).isEqualTo(0)

    assertThat(f.numberOfClaimsAt(0, 3)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(1, 3)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(2, 3)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(3, 3)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(4, 3)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(5, 3)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(6, 3)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(7, 3)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(8, 3)).isEqualTo(0)

    assertThat(f.numberOfClaimsAt(1, 4)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(2, 4)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(3, 4)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(4, 4)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(5, 4)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(6, 4)).isEqualTo(1)
    assertThat(f.numberOfClaimsAt(7, 4)).isEqualTo(0)

    f.mark(c.copy(id = 2))
    assertThat(f.numberOfClaimsAt(1, 4)).isEqualTo(0)
    assertThat(f.numberOfClaimsAt(2, 4)).isEqualTo(2)

    val f2 = Fabric()
    f2.mark(Claim(3, 0, 0, 1, 1))
    f2.mark(Claim(5, 0, 0, 1, 1))
    f2.mark(Claim(4, 100, 99, 1, 1))
    assertThat(f2.totalCoveredMoreThanNTimes(0)).isEqualTo(2)
    assertThat(f2.totalCoveredMoreThanNTimes(1)).isEqualTo(1)
}

fun testParsing() {
    val s = "#1 @ 1,3: 4x4"

    assertThat(parse(s)).isEqualTo(Claim(1, 1, 3, 4, 4))
}

fun parse(s: String): Claim {
    val groups = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".toRegex()
        .matchEntire(s)
        ?.groups ?: throw Exception("\"$s\" did not match expected format")

    return Claim(groups[1]!!.value.toInt(),
        groups[2]!!.value.toInt(),
        groups[3]!!.value.toInt(),
        groups[4]!!.value.toInt(),
        groups[5]!!.value.toInt())
}

fun example() {
    val input = listOf("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2")

    val f = Fabric()
    input
        .map { parse(it) }
        .forEach { f.mark(it) }

    assertThat(f.totalCoveredMoreThanNTimes(1)).isEqualTo(4)
}

const val DIMENSION = 2000

data class Claim(val id: Int,
                 val col: Int,
                 val row: Int,
                 val width: Int,
                 val height: Int)

class Fabric {
    private val squares: Array<Array<Set<Int>>> = Array(DIMENSION) { Array(DIMENSION) { setOf<Int>() } }

    fun mark(claim: Claim) {
        var x = 0
        for (row in claim.row until (claim.row + claim.height)) {
            for (col in claim.col until (claim.col + claim.width)) {
                if (row < 0 || col < 0 || row >= DIMENSION || col >= DIMENSION) {
                    throw IllegalArgumentException("out of bounds")
                }
                squares[row][col] = squares[row][col].plus(claim.id)
                x++
            }
        }
        if (x != claim.height * claim.width) {
            throw Exception("incorrect number of squares covered")
        }
    }

    fun numberOfClaimsAt(row: Int, col: Int): Int = squares[row][col].size

    fun totalCoveredMoreThanNTimes(n: Int): Int =
        squares
            .flatMap { it.toList() }
            .count { it.size > n }

    fun soloClaim(): Int {
        val hadSolo: MutableSet<Int> = mutableSetOf()
        val hadMulti: MutableSet<Int> = mutableSetOf()
        squares
            .flatMap { it.toList() }
            .forEach {
                if (it.size == 1){
                    hadSolo.addAll(it)
                } else if (it.size > 1) {
                    hadMulti.addAll(it)
                }
            }

        val onlySolo = hadSolo.minus(hadMulti)
        return onlySolo.first()
    }

}
