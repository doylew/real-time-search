package edu.unh.cs.ai.realtimesearch.planner.suboptimal

import edu.unh.cs.ai.realtimesearch.MetronomeException
import edu.unh.cs.ai.realtimesearch.environment.*
import edu.unh.cs.ai.realtimesearch.environment.Domain
import edu.unh.cs.ai.realtimesearch.experiment.configuration.Configurations
import edu.unh.cs.ai.realtimesearch.experiment.configuration.GeneralExperimentConfiguration
import edu.unh.cs.ai.realtimesearch.experiment.configuration.InvalidFieldException
import edu.unh.cs.ai.realtimesearch.experiment.terminationCheckers.TerminationChecker
import edu.unh.cs.ai.realtimesearch.planner.classical.ClassicalPlanner
import edu.unh.cs.ai.realtimesearch.planner.exception.GoalNotReachableException
import edu.unh.cs.ai.realtimesearch.util.*
import java.util.*
import kotlin.Comparator

class ExplicitEstimationSearch<StateType : State<StateType>>(val domain: Domain<StateType>, val configuration: GeneralExperimentConfiguration) : ClassicalPlanner<StateType>() {
    private val weight: Double = configuration.getTypedValue(Configurations.WEIGHT.toString()) ?: throw InvalidFieldException("\"${Configurations.WEIGHT}\" is not found. Please add it to the experiment configuration")

    private val cleanupNodeComparator = Comparator<ExplicitEstimationSearch.Node<StateType>> { lhs, rhs ->
        when {
            lhs.f < rhs.f -> -1
            lhs.f > rhs.f -> 1
            lhs.cost > rhs.cost -> -1
            lhs.cost < rhs.cost -> 1
            else -> 0
        }
    }

    private val focalNodeComparator = Comparator<ExplicitEstimationSearch.Node<StateType>> { lhs, rhs ->
        when {
            lhs.dHat < rhs.dHat -> -1
            lhs.dHat > rhs.dHat -> 1
            lhs.fHat < rhs.fHat -> -1
            lhs.fHat > rhs.fHat -> 1
            lhs.cost > rhs.cost -> -1
            lhs.cost < rhs.cost -> 1
            else -> 0
        }
    }

    private val openNodeComparator = Comparator<ExplicitEstimationSearch.Node<StateType>> { lhs, rhs ->
        when {
            lhs.fHat < rhs.fHat -> -1
            lhs.fHat > rhs.fHat -> 1
            lhs.d < rhs.d -> -1
            lhs.d > rhs.d -> 1
            lhs.cost > rhs.cost -> -1
            lhs.cost < rhs.cost -> 1
            else -> 0
        }
    }

    private val explicitNodeComparator = Comparator<ExplicitEstimationSearch.Node<StateType>> { lhs, rhs ->
        when {
            lhs.fHat < weight * rhs.fHat -> -1
            lhs.fHat > weight * rhs.fHat -> 1
            else -> 0
        }
    }

    private val rbTree = TreeMap<Node<StateType>, Node<StateType>>(openNodeComparator) //RedBlackTree(openNodeComparator, explicitNodeComparator)
    private val focal = AdvancedPriorityQueue(100000000, focalNodeComparator)

    private val FOCAL_ID = 1
    private val CLEANUP_ID = 0

    private val nodes: HashMap<StateType, ExplicitEstimationSearch.Node<StateType>> = HashMap<StateType, ExplicitEstimationSearch.Node<StateType>>(100000000, 1.toFloat()).resize()
    private val openList = ExplicitQueue(rbTree, focal, FOCAL_ID, explicitNodeComparator)
    private val cleanup = AdvancedPriorityQueue(100000000, cleanupNodeComparator)


    class ExplicitQueue<E>(val open: TreeMap<E, E>, val focal: AdvancedPriorityQueue<E>, private val id: Int,
                           private val explicitComparator: Comparator<E>) where E : RedBlackTreeElement<E, E>, E : Indexable {

        private class FocalVisitor<E>(val focal: AdvancedPriorityQueue<E>) : RedBlackTreeVisitor<E> where E : RedBlackTreeElement<E, E>, E : Indexable {
            private val ADD = 0
            private val REMOVE = 1

            override fun visit(k: E, op: Int) {
                when (op) {
                    ADD -> if (k.index == -1) focal.add(k)
                    REMOVE -> focal.remove(k)
                }
            }
        }

        private val focalVisitor = FocalVisitor(focal)

        fun isEmpty(): Boolean = open.firstEntry().value == null

        fun isNotEmpty(): Boolean = !isEmpty()

        fun add(e: E, oldBest: E) {
            open[e] = e
            if (explicitComparator.compare(e, oldBest) <= 0) {
                focal.add(e)
            }
        }

        fun updateFocal(oldBest: E?, newBest: E?, fHatChange: Int) {

//            if (oldBest == null || fHatChange != 0) {
//                if (oldBest != null && fHatChange < 0) {
//                    open.visit(newBest, oldBest, 1, focalVisitor)
//                } else if (oldBest?.getNode() == null) {
//                    open.visit(oldBest, newBest, 0, focalVisitor)
//                }
//            }
        }

        fun remove(e: E) {
            open.remove(e)
            if (e.index != -1) {
                focal.remove(e)
            }
        }

        fun pollOpen(): E? {
            val e = open.pollFirstEntry().value
            if (e != null && e.index != -1) {
                focal.remove(e)
            }
            return e
        }

        fun pollFocal(): E? {
            val e = focal.pop()
            if (e != null) {
                open.remove(e)
            }
            return e
        }

        fun peekOpen(): E? = open.firstEntry().value
        fun peekFocal(): E? = focal.peek()
    }

    class Node<StateType : State<StateType>>(val state: StateType, var heuristic: Double, var cost: Long,
                                             var actionCost: Long, var action: Action, var d: Double,
                                             var parent: ExplicitEstimationSearch.Node<StateType>? = null) : Indexable, RedBlackTreeElement<Node<StateType>, Node<StateType>>, Comparable<Node<StateType>> {

        private var redBlackNode: RedBlackTreeNode<Node<StateType>, Node<StateType>>? = null

        val f: Double
            get() = cost + heuristic

        private val depth: Int = parent?.depth?.plus(1) ?: 0

        var sseH = 0.0

        var sseD = 0.0

        var fHat = 0.0

        var hHat = 0.0

        var dHat = 0.0

        override var index: Int = -1

        init {
            computePathHats(parent, actionCost.toDouble())
        }

        private fun computePathHats(parent: Node<StateType>?, edgeCost: Double) {
            if (parent != null) {
                this.sseH = parent.sseH + ((edgeCost + heuristic) - parent.heuristic)
                this.sseD = parent.sseD + ((1 + d) - parent.d)
            }
            this.hHat = computeHHat()
            this.dHat = computeDHat()
            this.fHat = cost + hHat

            assert(fHat >= f)
            assert(dHat >= 0)
        }

        private fun computeHHat(): Double {
            var hHat = Double.MAX_VALUE
            val sseMean = if (cost == 0L) sseH else sseH / depth
            val dMean = if (cost == 0L) sseD else sseD / depth
            if (dMean < 1) {
                hHat = heuristic + ((d / (1 - dMean)) * sseMean)
            }
            return hHat
        }

        private fun computeDHat(): Double {
            var dHat = Double.MAX_VALUE
            val dMean = if (cost == 0L) sseD else sseD / depth
            if (dMean < 1) {
                dHat = d / (1 - dMean)
            }
            return dHat
        }

        override fun compareTo(other: Node<StateType>): Int {
            val diff = (this.f - other.f).toInt()
            if (diff == 0) return (other.cost - this.cost).toInt()
            return diff
        }

        override fun getNode(): RedBlackTreeNode<Node<StateType>, Node<StateType>>? {
            return redBlackNode
        }

        override fun setNode(node: RedBlackTreeNode<Node<StateType>, Node<StateType>>?) {
            this.redBlackNode = node
        }

    }

    private fun insertNode(node: Node<StateType>, oldBestNode: Node<StateType>) {
        openList.add(node, oldBestNode)
        cleanup.add(node)
        nodes[node.state] = node
    }

    private fun selectNode(): Node<StateType> {
        val bestDHat = openList.peekFocal() ?: throw MetronomeException("Focal is Empty!")
        val bestFHat = openList.peekOpen() ?: throw MetronomeException("Open is Empty!")
        val bestF = cleanup.peek() ?: throw MetronomeException("Cleanup is Empty!")

        when {
            bestDHat.fHat <= weight * bestF.f -> {
                val chosenNode = openList.pollFocal() ?: throw MetronomeException("Focal is Empty!")
                cleanup.remove(chosenNode)
                return chosenNode
            }
            bestFHat.fHat <= weight * bestF.f -> {
                val chosenNode = openList.pollOpen() ?: throw MetronomeException("Open is Empty!")
                cleanup.remove(chosenNode)
                return chosenNode
            }
            else -> {
                val chosenNode = cleanup.pop() ?: throw MetronomeException("Cleanup is Empty!")
                openList.remove(chosenNode)
                return chosenNode
            }
        }
    }

    private fun initializeAStar(): Long = System.currentTimeMillis()

    private fun getNode(sourceNode: Node<StateType>, successorBundle: SuccessorBundle<StateType>): Node<StateType> {
        val successorState = successorBundle.state
        val tempSuccessorNode = nodes[successorState]
        return if (tempSuccessorNode == null) {
            generatedNodeCount++
            val undiscoveredNode = Node(
                    state = successorState,
                    heuristic = weight * domain.heuristic(successorState),
                    actionCost = successorBundle.actionCost,
                    action = successorBundle.action,
                    parent = sourceNode,
                    cost = Long.MAX_VALUE,
                    d = sourceNode.d + (domain.heuristic(successorState) / successorBundle.actionCost).toInt()
            )
            nodes[successorState] = undiscoveredNode
            undiscoveredNode
        } else {
            tempSuccessorNode
        }
    }

    private fun expandFromNode(sourceNode: Node<StateType>) {
        expandedNodeCount++
        val currentGValue = sourceNode.cost
        for (successor in domain.successors(sourceNode.state)) {
            val successorState = successor.state
            val successorNode = getNode(sourceNode, successor)
            // skip if we have our parent as a successor
            if (successorState == sourceNode.parent?.state) {
                continue
            }

            // only generate states which have not been visited or with a cheaper cost
            val successorGValueFromCurrent = currentGValue + successor.actionCost
            if (successorNode.cost > successorGValueFromCurrent) {
                assert(successorNode.state == successor.state)
                successorNode.apply {
                    cost = successorGValueFromCurrent
                    parent = sourceNode
                    action = successor.action
                    actionCost = successor.actionCost
                }
                if (!successorNode.open) {
                    openList.add(successorNode, successorNode)
                    insertNode(successorNode, successorNode)
                } else {
                    openList.focal.update(successorNode)
                    insertNode(successorNode, successorNode)
                }
            }
        }
    }

    private fun extractPlan(solutionNode: Node<StateType>, startState: StateType): List<Action> {
        val actions = arrayListOf<Action>()
        var iterationNode = solutionNode
        while (iterationNode.parent != null) {
            actions.add(iterationNode.action)
            iterationNode = iterationNode.parent!!
        }
        assert(startState == iterationNode.state )
        actions.reverse()
        return actions
    }

    override fun plan(state: StateType, terminationChecker: TerminationChecker): List<Action> {
        val startTime = initializeAStar()
        val node = Node(state, weight * domain.heuristic(state), 0, 0, NoOperationAction, d = 0.0)
//        var currentNode: Node<StateType>
        nodes[state] = node
        cleanup.add(node)
        openList.add(node, node)
        generatedNodeCount++

        while (openList.isNotEmpty() && !terminationChecker.reachedTermination()) {
            val oldBest = openList.peekOpen()
            val topNode = selectNode() // openList.peek() ?: throw GoalNotReachableException("Open list is empty")
            if (domain.isGoal(topNode.state)) {
                executionNanoTime = System.currentTimeMillis() - startTime
                return extractPlan(topNode, state)
            }
            expandFromNode(topNode)
            val newBest = openList.peekOpen()
            val fHatChange = openNodeComparator.compare(newBest, oldBest)
            openList.updateFocal(oldBest, newBest, fHatChange)
            terminationChecker.notifyExpansion()
        }
        throw GoalNotReachableException()
    }
}
