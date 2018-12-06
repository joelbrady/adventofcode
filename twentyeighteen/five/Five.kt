package twentyeighteen.five

import assertThat
import java.io.File

fun main() {
    tests()

    example()

    problem()
}

fun tests() {
    assertThat("ab".substring(0, 2)).isEqualTo("ab")

    assertThat(react("ab")).isEqualTo("ab")
    assertThat(react("aA")).isEqualTo("")
    assertThat(react("Aa")).isEqualTo("")
    assertThat(react("aAa")).isEqualTo("a")
    assertThat(react("aAbB")).isEqualTo("")
    assertThat(react("aAbBa")).isEqualTo("a")

    assertThat(reactWithout("acccccA", 'c')).isEqualTo("")
    assertThat(reactWithout("accccc", 'c')).isEqualTo("a")
}

fun example() {
    val polymer = "dabAcCaCBAcCcaDA"

    assertThat(react(polymer)).isEqualTo("dabCBAcaDA")

    assertThat(reactWithout(polymer, 'a')).isEqualTo("dbCBcD")
    assertThat(reactWithout(polymer, 'b')).isEqualTo("daCAcaDA")
    assertThat(reactWithout(polymer, 'c')).isEqualTo("daDA")
    assertThat(reactWithout(polymer, 'd')).isEqualTo("abCBAc")
}

fun problem() {
    File("5.input")
        .bufferedReader()
        .useLines { lines ->
            val firstLine = lines.first()
            partA(firstLine)
            partB(firstLine)
        }
}

fun partA(input: String) {
    val finalPolymer = react(input)
    println("Part A: ${finalPolymer.length}")
}

fun partB(input: String) {
    val charatersPresent = input
        .toLowerCase()
        .toSet()

    val simplifiedResults = charatersPresent
        .map { reactWithout(input, it) }

    val shortestPolymer = simplifiedResults
        .minBy { it.length }

    println("Part B: ${shortestPolymer!!.length}")
}

fun reactWithout(polymer: String, atom: Char): String {
    val simplifiedPolymer = polymer
        .replace(atom.toLowerCase().toString(), "")
        .replace(atom.toUpperCase().toString(), "")

    return react(simplifiedPolymer)
}

tailrec fun react(polymer: String): String {
    if (polymer.length <= 1) {
        return polymer
    }

    for (i in 0 until polymer.length - 1) {
        if (currentPairMatch(polymer, i)) {
            val pair = polymer.substring(i, i + 2)
            val polymerWithPairRemoved = polymer.replace(pair, "")
            return react(polymerWithPairRemoved)
        }
    }

    return polymer
}

fun currentPairMatch(polymer: String, index: Int): Boolean {
    val a = polymer[index]
    val b = polymer[index + 1]
    return a != b && a.toUpperCase() == b.toUpperCase()
}
