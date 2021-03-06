package twentyeighteen2.nine

import assertThat
import java.math.BigInteger
import java.util.PriorityQueue

const val debug = false

fun main() {
    tests()
    mutableTests()
    examples()
    problem()
}

fun problem() {
    // 465 players; last marble is worth 71498 points
    val answerA = playGame(465, 71498)
    println("Part A: $answerA")
    val answerB = playMutableGame(465, 71498 * 100)
    println("Part B: $answerB")
}

fun examples() {
    assertThat(playGame(9, 25)).isEqualTo(32)
    assertThat(playGame(10, 1618)).isEqualTo(8317)
    assertThat(playGame(13, 7999)).isEqualTo(146373)
    assertThat(playGame(17, 1104)).isEqualTo(2764)
    assertThat(playGame(21, 6111)).isEqualTo(54718)
    assertThat(playGame(30, 5807)).isEqualTo(37305)
}

fun tests() {
    assertThat(PriorityQueue(listOf(0, 1, 2, 3)).poll()).isEqualTo(0)

    assertThat((-7).nonNegativeMod(5)).isEqualTo(3)
    assertThat(5.nonNegativeMod(5)).isEqualTo(0)
    assertThat(7.nonNegativeMod(5)).isEqualTo(2)
    assertThat((-12).nonNegativeMod(5)).isEqualTo(3)

    assertThat(initialState().insert(1).state).isEqualTo(MarblesState(listOf(0, 1), 1))
    assertThat(initialState()
        .insert(1).state
        .insert(2).state)
        .isEqualTo(MarblesState(listOf(0, 2, 1), 1))
    assertThat(initialState()
        .insert(1).state
        .insert(2).state
        .insert(3).state)
        .isEqualTo(MarblesState(listOf(0, 2, 1, 3), 3))

    /*
        [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
        [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
     */

    val before23 = MarblesState(parseExampleLine("0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15"), 13)
    val after23 = MarblesState(parseExampleLine("0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15"), 6)

    assertThat(before23.insert(23)).isEqualTo(MarblesState.InsertionResult(after23, setOf(23, 9)))
}

fun playGame(numPlayers: Int, lastMarble: Int): Int {
    val a = playImmutableGame(numPlayers, lastMarble)
    if (debug) {
        println("\nStarting mutable game")
    }
    val b = playMutableGame(numPlayers, lastMarble).toInt()
    assertThat(a).isEqualTo(b)
    return a
}

fun playImmutableGame(numPlayers: Int, lastMarble: Int): Int {
    if (lastMarble < 1) {
        throw IllegalArgumentException("expect there to be at least 2 marbles in the game")
    }
    val scores: Map<Int, Int> = (1..numPlayers).map { it to 0 }.toMap()
    val marbles: List<Int> = (1..lastMarble).toList()

    return turn(initialState(), marbles, scores, 1)
}

tailrec fun turn(state: MarblesState, marbles: List<Int>, scores: Map<Int, Int>, currentPlayer: Int): Int {
    if (marbles.isEmpty()) {
        return scores.values.max() ?: TODO()
    }
    val maxPlayerNum = scores.keys.max() ?: TODO()
    val marble = marbles.first()
    val remainingMarbles = marbles.subList(1, marbles.size)
    val (nextState, toScore) = state.insert(marble)
    if (debug && toScore.isNotEmpty()) {
        println("Player $currentPlayer scores $toScore")
    }
    val newScores = scores.plus(currentPlayer to (scores.getOrDefault(currentPlayer, 0) + toScore.sum()))
    val nextPlayer = if (currentPlayer < maxPlayerNum) currentPlayer + 1 else 1
    return turn(nextState, remainingMarbles, newScores, nextPlayer)
}

fun parseExampleLine(s: String) = s
    .replace("(", " ")
    .replace(")", " ")
    .split(" +".toRegex())
    .toList()
    .map { it.toInt() }

data class MarblesState(val marbles: List<Int>, val current: Int) : Snapshotable {
    data class InsertionResult(val state: MarblesState, val marblesToScore: Set<Int>)

    fun insert(marble: Int): InsertionResult {
        if (marble % 23 == 0) {
            val removeIndex = (current - 7).nonNegativeMod(marbles.size)
            val before = marbles.subList(0, removeIndex)
            val after = if ((removeIndex + 1) < marbles.size) marbles.subList(removeIndex + 1, marbles.size) else listOf()
            val removedMarble = marbles[removeIndex]
            val newMarbles = before.plus(after)
            val newCurrentIndex = (removeIndex) % newMarbles.size
            return InsertionResult(MarblesState(newMarbles, newCurrentIndex), setOf(marble, removedMarble))
        } else {
            val indexToRight = (current + 1) % marbles.size
            val insertionIndex = indexToRight + 1
            val beforeInsertionPoint = marbles.subList(0, insertionIndex)
            val afterInsertionPoint = marbles.subList(insertionIndex, marbles.size)
            val newState = MarblesState(beforeInsertionPoint.plus(marble).plus(afterInsertionPoint), insertionIndex)
            return InsertionResult(newState, setOf())
        }
    }

    override fun snapshot(): List<Int> {
        return marbles.subList(current, marbles.size).plus(marbles.subList(0, current))
    }
}

fun initialState(): MarblesState = MarblesState(listOf(0), 0)

fun Int.nonNegativeMod(modulus: Int): Int {
    var n = this % modulus
    while (n < 0) {
        n += modulus
    }
    return n
}

class MutableMarbles : Snapshotable {
    private class Node(val value: Int) {
        var left: Node = this
        var right: Node = this
    }

    private var current = Node(0)
    init {
        current.left = current
        current.right = current
    }

    // returns the set of marbles to be scored
    fun insert(marble: Int): Set<Int> {
        if (marble % 23 == 0) {
            var n = current
            for (ignored in 1..7) {
                n = n.left
            }
            current = n.right
            n.left.right = n.right
            n.right.left = n.left
            return setOf(n.value, marble)
        } else {
            val n = current.right
            val newNode = Node(marble)
            newNode.left = n
            newNode.right = n.right
            n.right.left = newNode
            n.right = newNode
            current = newNode
            return setOf()
        }
    }

    override fun snapshot(): List<Int> {
        val list: MutableList<Int> = mutableListOf(current.value)
        var n = current.right
        while (n != current) {
            list.add(n.value)
            n = n.right
        }
        return list
    }
}

fun playMutableGame(numPlayers: Int, lastMarble: Int): BigInteger {
    if (lastMarble < 1) {
        throw IllegalArgumentException("expect there to be at least 2 marbles in the game")
    }
    val scores: MutableMap<Int, BigInteger> = mutableMapOf()
    val marbles: List<Int> = (1..lastMarble).toList()

    val m = MutableMarbles()
    var currentPlayer = 1

    for (marble in marbles) {
        val toScore = m.insert(marble)
        if (debug && toScore.isNotEmpty()) {
            println("Player $currentPlayer scores $toScore")
        }
        scores[currentPlayer] = scores.getOrDefault(currentPlayer, BigInteger.ZERO) + toScore.sum().toBigInteger()
        currentPlayer++
        if (currentPlayer > numPlayers) {
            currentPlayer = 1
        }
    }

    return scores.values.max() ?: TODO()
}

fun mutableTests() {
    val m = MutableMarbles()
    val a = m.insert(1)
    assertThat(a.isEmpty()).isTrue()
    for (marble in 2..22) {
        val b = m.insert(marble)
        assertThat(b.isEmpty()).isTrue()
    }
    val c = m.insert(23)
    assertThat(c).isEqualTo(setOf(23, 9))
}

interface Snapshotable {
    fun snapshot(): List<Int>
}
