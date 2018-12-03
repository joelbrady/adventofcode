import java.io.File

fun main() {
    tests()

    File("2.input")
        .bufferedReader()
        .useLines { lines ->
            println(lines)
            val reusableLines = lines.toList()
            twoA(reusableLines)
            twoB(reusableLines)
        }

}

fun twoA(lines: List<String>) {
    println("Answer to part A: ${checkSum(lines.filter { it.isNotEmpty() })}")
}

fun twoB(lines: List<String>) {
    val answer = lines
        .flatMap { a -> lines.map { b -> Pair(a, b) } }
        .filter { (a, b) -> differBy(a, b) == 1 }
        .map { (a, b) -> commonLetters(a, b) }
        .first()

    println("Answer to part B: $answer")
}

fun differBy(a: String, b: String): Int =
    a.zip(b)
        .map { (a, b) -> if (a != b) 1 else 0 }
        .sum()

fun tests() {
    assertThat(twoTimes("abcdef")).isFalse()
    assertThat(threeTimes("abcdef")).isFalse()
    assertThat(twoTimes("bababc")).isTrue()
    assertThat(threeTimes("bababc")).isTrue()
    assertThat(twoTimes("abbcde")).isTrue()
    assertThat(threeTimes("abbcde")).isFalse()

    assertThat(checkSum("abcdef", "bababc", "abbcde")).isEqualTo(2 * 1)

    assertThat(differBy("abc", "bbc")).isEqualTo(1)
    assertThat(differBy("a", "a")).isEqualTo(0)
    assertThat(differBy("ab", "cd")).isEqualTo(2)

    assertThat(commonLetters("abc", "abc")).isEqualTo("abc")
    assertThat(commonLetters("abc", "bbc")).isEqualTo("bc")
}

fun commonLetters(a: String, b: String): String =
    a.zip(b)
        .filter { (a, b) -> a == b }
        .map { (a, _) -> a }
        .joinToString("")

fun checkSum(vararg s: String): Int {
    return checkSum(s.toList())
}

fun checkSum(ss: List<String>): Int {
    val twos = ss
        .filter { twoTimes(it) }
        .count()
    val threes = ss
        .filter { threeTimes(it) }
        .count()

    return twos * threes
}

fun twoTimes(s: String): Boolean = nTimes(s, 2)
fun threeTimes(s: String): Boolean = nTimes(s, 3)

fun nTimes(s: String, n: Int): Boolean {
    return s.split("")
        .filter { it.isNotEmpty() }
        .groupingBy { it }
        .eachCount()
        .values
        .contains(n)
}

fun assertThat(a: Any): Assertion {
    return Assertion(a)
}

class Assertion(private val a: Any?) {
    fun isTrue() {
        if (a != true) {
            throw AssertionError("$a is not true")
        }
    }

    fun isFalse() {
        if (a != false) {
            throw AssertionError("$a is not false")
        }
    }

    fun isEqualTo(b: Any?) {
        if (a != b) {
            throw AssertionError("$a is not equal to $b")
        }
    }
}

class AssertionError(message: String) : Exception(message)
