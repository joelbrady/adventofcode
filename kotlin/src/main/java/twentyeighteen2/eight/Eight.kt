package twentyeighteen2.eight

import assertThat
import java.io.File

fun main() {
    tests()

    problem()
}

fun problem() {
    File("8.input.txt")
        .bufferedReader()
        .useLines {
            val input = it.toList()
            val (root, _) = parseNode(input.first())
            val answerA = root.sumOfMetadata()
            println("Part A: $answerA")
            val answerB = root.value()
            println("Part B: $answerB")
        }
}

fun tests() {
    assertThat(d.sumOfMetadata()).isEqualTo(99)
    assertThat(c.sumOfMetadata()).isEqualTo(99 + 2)
    assertThat(b.sumOfMetadata()).isEqualTo(10 + 11 + 12)
    assertThat(a.sumOfMetadata()).isEqualTo(138)

    assertThat(parseNode("0 1 1").value).isEqualTo(Node(listOf(), listOf(1)))
    assertThat(parseNode("0 2 1 2").value).isEqualTo(Node(listOf(), listOf(1, 2)))
    assertThat(parseNode("1 1 0 1 5 9").value).isEqualTo(Node(listOf(Node(listOf(), listOf(5))), listOf(9)))
    assertThat(parseNode("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2").value).isEqualTo(a)

    assertThat(d.value()).isEqualTo(99)
    assertThat(b.value()).isEqualTo(10 + 11+ 12)
    assertThat(c.value()).isEqualTo(0)
    assertThat(a.value()).isEqualTo(66)
}

typealias Metadata = List<Int>

// metadata must be at least 1 entry
data class Node(val children: List<Node>, val metadata: Metadata) {
    fun sumOfMetadata(): Int = metadata.sum() + children.map { it.sumOfMetadata() }.sum()

    fun value(): Int {
        if (children.isEmpty()) {
            return metadata.sum()
        }

        return metadata
            .map { it - 1 }
            .filter { it >= 0 }
            .mapNotNull { children.elementAtOrNull(it) }
            .map { it.value() }
            .sum()
    }
}

val d = Node(listOf(), listOf(99))
val c = Node(listOf(d), listOf(2))
val b = Node(listOf(), listOf(10, 11, 12))
val a = Node(listOf(b, c), listOf(1, 1, 2))

data class ParseResult<T>(val value: T, val s: String)

fun parseNodes(s: String, n: Int): ParseResult<List<Node>> {
    val children: MutableList<Node> = mutableListOf()
    var remainder = s
    for (ignored in 1..n) {
        val (c, rr) = parseNode(remainder)
        children.add(c)
        remainder = rr
    }

    return ParseResult(children, remainder)
}

fun parseNode(s: String): ParseResult<Node> {
    val (numChildren, r1) = parseInt(s)
    val (numMetadata, r2) = parseInt(r1)

    var children: List<Node> = listOf()
    var r3 = r2
    if (numChildren > 0) {
        val (parsedChildren, rChildren) = parseNodes(r2, numChildren)
        children = parsedChildren
        r3 = rChildren
    }

    val (metadata, r4) = parseMetadata(r3, numMetadata)

    return ParseResult(Node(children, metadata), r4)
}

fun parseInt(s: String): ParseResult<Int> {
    val (count) = "^(\\d+ ?).*".toRegex().matchEntire(s)!!.destructured
    return ParseResult(count.removeSuffix(" ").toInt(), s.replaceFirst(count, ""))
}

fun parseMetadata(s: String, n: Int): ParseResult<Metadata> {
    val metadata: MutableList<Int> = mutableListOf()
    var remainder = s
    for (ignored in 1..n) {
        val (m, r2) = parseInt(remainder)
        metadata.add(m)
        remainder = r2
    }

    return ParseResult(metadata, remainder)
}
