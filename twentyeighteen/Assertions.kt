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
            throw AssertionError("Expected $a to equal $b")
        }
    }
}

class AssertionError(message: String) : Exception(message)
