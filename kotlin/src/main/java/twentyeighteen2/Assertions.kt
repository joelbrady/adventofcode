fun assertThat(actual: Any): Assertion {
    return Assertion(actual)
}

class Assertion(private val actual: Any?) {
    fun isTrue() {
        if (actual != true) {
            throw AssertionError("$actual is not true")
        }
    }

    fun isFalse() {
        if (actual != false) {
            throw AssertionError("$actual is not false")
        }
    }

    fun isEqualTo(expected: Any?) {
        if (actual != expected) {
            throw AssertionError("Expected $actual to equal $expected")
        }
    }
}

class AssertionError(message: String) : Exception(message)
