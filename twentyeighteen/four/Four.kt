package twentyeighteen.four

import assertThat
import java.io.File

fun main() {
    tests()

    example()

    problem()
}

fun problem() {
    File("4.input")
        .bufferedReader()
        .useLines {
            val answer = solve(it.toList().sorted())
            println("Part A: ${answer.a}")
            println("Part B: ${answer.b}")
        }
}

fun example() {
    val input =
"""[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""
    .split("\n")
    .toList()

    val answer = solve(input)

    assertThat(answer.a).isEqualTo(240)
}

data class Answer(val a: Int, val b: Int)

fun solve(input: List<String>): Answer {
    var currentGuard = ""
    var start: Int = -1
    val guards: MutableMap<String, Guard> = mutableMapOf()
    input.forEach {
        val guardRegex =".*Guard #(\\d+) begins shift".toRegex().matchEntire(it)
        val fallsAsleep = ".*:(\\d{2})] falls asleep".toRegex().matchEntire(it)
        val wakesUp = ".*:(\\d{2})] wakes up".toRegex().matchEntire(it)
        when {
            guardRegex != null -> currentGuard = guardRegex.groups[1]!!.value
            fallsAsleep != null -> start = fallsAsleep.groups[1]!!.value.toInt()
            wakesUp != null -> {
                val end = wakesUp.groups[1]!!.value.toInt()
                val g = guards.getOrDefault(currentGuard, Guard(currentGuard))
                g.sleep(start, end)
                guards[currentGuard] = g
            }
        }
    }

    val allGuards = guards.values
    val a = solveA(allGuards)
    val b = solveB(allGuards)
    return Answer(a, b)
}

fun solveA(allGuards: Collection<Guard>): Int {
    val guardThatSleptTheMost = allGuards.maxWith(Comparator { a, b -> a.totalMinutesAsleep() - b.totalMinutesAsleep() } )
    return guardThatSleptTheMost!!.let { it.mostCommonlySleptMinute() * it.id.toInt() }
}

fun solveB(allGuards: Collection<Guard>): Int {
    val m = allGuards
        .groupBy { it.mostOften().times }

    val biggestFrequency = m.keys.maxWith(Comparator { a, b -> a - b })
    val guard = m[biggestFrequency]!!.first()
    return guard.id.toInt() * guard.mostCommonlySleptMinute()
}

fun tests() {
    guardTests()
}

fun guardTests() {
    val g = Guard("1")
    g.sleep(1, 10)
    g.sleep(2, 3)
    g.sleep(2, 3)

    assertThat(g.totalMinutesAsleep()).isEqualTo(11)
    assertThat(g.mostCommonlySleptMinute()).isEqualTo(2)
    assertThat(g.mostOften()).isEqualTo(Often(2, 3))
}

data class Often(val minute: Int, val times: Int)

class Guard(val id: String) {
    private val minutes: Array<Int> = Array(60) { 0 }

    fun sleep(start: Int, end: Int) {
        for (i in start until end) {
            minutes[i]++
        }
    }

    fun totalMinutesAsleep(): Int {
        return minutes.sum()
    }

    fun mostCommonlySleptMinute(): Int {
        var maxValue = -1
        var maxIndex = -1
        for (i in 0 until minutes.size) {
            if (minutes[i] > maxValue) {
                maxValue = minutes[i]
                maxIndex = i
            }
        }

        return maxIndex
    }

    fun mostOften(): Often {
        val index = mostCommonlySleptMinute()
        val times = minutes[index]
        return Often(index, times)
    }
}
