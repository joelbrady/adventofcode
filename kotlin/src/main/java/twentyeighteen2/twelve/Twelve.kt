package twentyeighteen2.twelve

import assertThat

fun main() {
    tests()
    examples()
    puzzle()
}

fun puzzle() {
    val rules = """####. => #
..#.. => .
#.#.. => .
.##.. => .
##... => .
#.##. => #
##.#. => .
##..# => .
.###. => .
.#.## => .
.#..# => #
..... => .
###.. => #
#..## => .
##.## => .
#.... => .
...## => #
....# => .
#.#.# => #
###.# => .
.#### => #
.#... => #
#.### => .
..### => .
.#.#. => #
.##.# => .
#..#. => #
...#. => .
#...# => #
..##. => .
##### => #
..#.# => #"""
        .split("\n")
        .map { it.toRule() }
        .let { Rules(it) }
    val initial = "#.#.#....##...##...##...#.##.#.###...#.##...#....#.#...#.##.........#.#...#..##.#.....#..#.###".toGeneration(rules = rules)

    val startTime = System.nanoTime()
    val twentiethGeneration = run(initial, 500000)
    val answerA = twentiethGeneration.state.sum()
    val duration = (System.nanoTime() - startTime) / 1000000000.0
    println("Part A: $answerA (took $duration seconds)")
//    val partB = run(initial, 50000000000L, rules)
//    val answerB = partB.state.sum()
//    println("Part B: $answerB")
}

fun tests() {
    val rule = "...## => #".toRule()
    assertThat(rule).isEqualTo(Rule(listOf(false, false, false, true, true), true))

    assertThat(rule.apply(0, setOf(1, 2))).isEqualTo(true)

    val state = ".#.#.#".toGeneration()
    assertThat(state).isEqualTo(Generation(setOf(1, 3, 5), Rules(listOf())))

    val r1 = Rules(listOf("..#.. => .".toRule()))
    assertThat("#....".toGeneration(rules = r1).next()).isEqualTo("......".toGeneration(rules = r1))

    val r2 = Rules(listOf("..#.. => #".toRule()))
    assertThat("#....#".toGeneration(rules = r2).next()).isEqualTo("#....#".toGeneration(rules = r2))
}

fun examples() {
    val rules = """...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"""
        .split("\n")
        .map { it.toRule() }
        .let { Rules(it) }

    val initialState = "#..#.#..##......###...###".toGeneration(rules = rules)

    val s2 = initialState.next()
    assertThat(s2).isEqualTo("#...#....#.....#..#..#..#...........".toGeneration(rules = initialState.rules))
    val s3 = s2.next()
    assertThat(s3).isEqualTo("...##..##...##....#..#..#..##..........".toGeneration(-3, rules = s2.rules))

    assertThat(run(initialState, 2)).isEqualTo(s3)

    assertThat(run(initialState, 20)).isEqualTo(".#....##....#####...#######....#.#..##.".toGeneration(-3, rules))
}

fun run(initial: Generation, numGenerations: Long): Generation = run(initial, numGenerations, mutableSetOf())

tailrec fun run(initial: Generation, numGenerations: Long, seen: MutableSet<String>): Generation {
    if (numGenerations <= 0) {
        return initial
    }

    val s = simple(initial.state)
    if (seen.contains(s)) {
        println("saw $s again at $numGenerations")
        System.exit(0)
    }

    seen.add(s)

    return run(initial.next(), numGenerations - 1, seen)
}

// show set of indexes as truncated pretty string for finding repeating patterns
fun simple(s: Set<Int>): String {
    val min = s.min() ?: TODO()
    val max = s.max() ?: TODO()
    val l = StringBuilder()
    for (i in min..max) {
        if (i in s) {
            l.append('#')
        } else {
            l.append('.')
        }
    }
    return l.toString()
}

data class Rule(val pattern: List<Boolean>, val result: Boolean) {
    // if the plant at plantPosition matches the rule, then we return result, otherwise false
    fun apply(plantPosition: Int, positions: Set<Int>): Boolean {
        val currentPlant = ((plantPosition - 2)..(plantPosition + 2)).map { it in positions }.toList()
        return if (currentPlant == pattern) result else false
    }
}

fun String.toRule(): Rule {
    if (this.length != 10) {
        TODO()
    }

    val patternString = this.substring(0..4)
    val result = toBoolean(this.last())
    val pattern: List<Boolean> = patternString.map { toBoolean(it) }.toList()

    return Rule(pattern, result)
}

fun toBoolean(c: Char): Boolean {
    return when (c) {
        '#' -> true
        '.' -> false
        else -> TODO()
    }
}

data class Rules(private val rs: List<Rule>) {

    // get rid of anything that results in death, same as no rule at all
    private val rules = rs.filter { it.result }

    private val cache: MutableMap<List<Boolean>, Boolean> = mutableMapOf()

    fun apply(plantPosition: Int, positions: Set<Int>): Boolean {
        val key = ((plantPosition - 2)..(plantPosition + 2)).map { it in positions }.toList()
        val cached = cache[key]
        if (cached != null) {
            return cached
        }
        val result = rules
            .any { it.apply(plantPosition, positions) }
        cache[key] = result
        return result
    }
}

data class Generation(val state: Set<Int>, val rules: Rules) {
    fun next(): Generation {
        val leftBound = (state.min() ?: TODO()) - 5
        val rightBound = (state.max() ?: TODO()) + 5
        return (leftBound..rightBound)
            .map { index -> index to rules.apply(index, state) }
            .filter { it.second }
            .map { it.first }
            .toSet()
            .let { Generation(it, rules) }
    }
}

fun String.toGeneration(startIndex: Int = 0, rules: Rules = Rules(listOf())): Generation =
    this
        .toList()
        .zip(startIndex until this.length)
        .filter { toBoolean(it.first) }
        .map { it.second }
        .toSet()
        .let { Generation(it, rules) }
