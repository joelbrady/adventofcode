import java.io.File
import java.lang.IllegalStateException

data class Tree(val name: String, val value: Int)

data class ParseResult(val node: Tree, val children: List<String>)

fun parse(line: String): ParseResult? {
    val regex = "(\\w+) \\((\\d+)\\)(.+)?".toRegex()
    val groups = regex.matchEntire(line)?.groups
    if (groups != null) {
        val parentName = groups[1]!!.value
        val value = Integer.parseInt(groups[2]!!.value)
        val children = groups[3]?.value.let { parseChildren(it) }
        val node = Tree(parentName, value)
        return ParseResult(node, children)
    }
    return null
}

fun parseChildren(children: String?): List<String> =
    children
        ?.replace(" -> ", "")
        ?.split(", ") ?: listOf()

fun main(args: Array<String>) {
    val nodeValues: MutableMap<String, Int> = mutableMapOf()
    val parentMap: MutableMap<String, String> = mutableMapOf()

    val filename = args[0]
    File(filename).bufferedReader().useLines { lines ->
        lines
            .map { parse(it) }
            .filterNotNull()
            .forEach { result ->
                val (node, children) = result
                nodeValues[node.name] = node.value
                children.forEach { child -> parentMap[child] = node.name }
            }
    }

    val rootName = findRootName(nodeValues, parentMap)

    println("Part A solution: $rootName")

    // we could use this for part A now as well
    val root = buildTree(rootName, nodeValues, parentMap)

    // the initial target weight doesn't matter since we have children
    println(f(root, -1).let { "Part B solution: ${it.weight}" })

}

data class Node(val name: String, val weight: Int, val children: List<Node>) {
    fun totalWeight(): Int {
        return weight + children.map { it.totalWeight() }.sum()
    }
}

/*
    b (2)  c (3)   d (2)
      \     |      /
       \    |     /
        \   |    /
         \  |   /
           a (x)  c should be 2
 */

// simple example for debugging
val c = Node("c", 3, listOf())
val b = Node("b", 2, listOf())
val d = Node("d", 2, listOf())
val a = Node("a", 1, listOf(b, c, d))

data class Result(val name: String, val weight: Int)

// given a tree and a target weight for the tree, return a new Node that
// contains the name of the node and it's new weight to fix the whole tree
fun f(node: Node, targetWeight: Int): Result {
    if (node.children.isEmpty()) {
        // i don't have any children, hence i need to change
        return Result(node.name, targetWeight)
    }

    val weights = buildWeights(node.children)

    if (weights.size == 1) {
        // all children have the same weight, must change myself
        val difference = targetWeight - node.totalWeight()
        return Result(node.name, node.weight + difference)
    }

    val newTargetWeight = weights
        .filter { it.value.size != 1 }
        .keys
        .first()

    val oddChild = weights
        .filter { it.value.size == 1 } // the child that is incorrect
        .values
        .first()
        .first()

    return f(oddChild, newTargetWeight)
}

fun buildWeights(children: List<Node>): Map<Int, List<Node>> {
    val weights: MutableMap<Int, List<Node>> = mutableMapOf()
    children.forEach {
        val weight = it.totalWeight()
        val l: List<Node> = weights.getOrDefault(weight, listOf())
        weights[weight] = l.plus(it)
    }

    return weights.toMap()
}

fun findRootName(nodeValues: Map<String, Int>, parentMap: Map<String, String>): String {
    var anyNode: String = nodeValues.keys.first()
    var parent: String? = parentMap[anyNode]
    while (parent != null) {
        anyNode = parent
        parent = parentMap[anyNode]
    }
    return anyNode
}

fun buildTree(rootName: String, nodeValues: Map<String, Int>, parentMap: Map<String, String>): Node {
    val childrenNames = parentMap
        .filter { it.value == rootName } // nodes whose parent is the root
        .keys

    return Node(
        rootName,
        nodeValues.getOrElse(rootName) { throw IllegalStateException("foo") },
        childrenNames.map { buildTree(it, nodeValues, parentMap) }
    )
}
