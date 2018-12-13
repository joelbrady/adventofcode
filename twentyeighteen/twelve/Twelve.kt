package twentyeighteen.twelve

import assertThat

fun main() {
    tests()
}

fun tests() {
    val rule = "...## => #".toRule()
    assertThat(rule).isEqualTo(Rule(listOf(false, false, false, true, true), true))

    assertThat(rule.apply(0, setOf(1, 2))).isEqualTo(true)

    val state = ".#.#.#".toState()
    assertThat(state).isEqualTo(setOf(1, 3, 5))
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

fun String.toState(): Set<Int> = this
    .toList()
    .zip(0 until this.length)
    .filter { toBoolean(it.first) }
    .map { it.second }
    .toSet()
