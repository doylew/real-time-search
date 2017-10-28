package edu.unh.cs.ai.realtimesearch.util

interface BucketNode {
    fun getFValue(): Double
    fun getGValue(): Double
    fun getHValue(): Double
    fun updateIndex(i: Int)
    override fun toString(): String
}

class BucketOpenList<T : BucketNode>(private val bound: Double, private var fMin: Double = Double.MAX_VALUE) {


    private val openList = AdvancedPriorityQueue<GHPair>(1000000, PotentialComparator(bound, fMin))
    private val lookUpTable = HashMap<GHPair, Bucket<T>>(1000000, 1.toFloat())
    private val pairLookUp = HashMap<Pair<Double, Double>, GHPair>(1000000, 1.toFloat())

    private class BucketOpenListException(message: String) : Exception(message)

    private class PotentialComparator<T>(var bound: Double, var fMin: Double) : Comparator<T> {
        override fun compare(leftBucket: T, rightBucket: T): Int {
            if (leftBucket != null && rightBucket != null) {
                if (leftBucket is GHPair && rightBucket is GHPair) {
                    var leftBucketPotential = ((bound * fMin) - leftBucket.g) / (leftBucket.h)
                    var rightBucketPotential = ((bound * fMin) - rightBucket.g) / (rightBucket.h)
                    if(leftBucket.h == 0.0) leftBucketPotential = Double.MAX_VALUE
                    if(rightBucket.h == 0.0) rightBucketPotential = Double.MAX_VALUE

                    // First compare by potential (bigger is preferred), then by f (smaller is preferred), then by g (smaller is preferred)
                    if (leftBucketPotential > rightBucketPotential) return -1
                    if (leftBucketPotential < rightBucketPotential) return 1

                    if (leftBucket.g < rightBucket.g) return -1
                    if (leftBucket.g > rightBucket.g) return 1

                    if (leftBucket.h < rightBucket.h) return -1
                    if (leftBucket.h > rightBucket.h) return 1

                    return 0
                }
            }
            throw BucketOpenListException("Null buckets or non-bucket elements on open.")
        }
    }

    private data class GHPair(val g: Double, val h: Double) : Indexable {
        override var index: Int = -1

        val f
            get() = g + h
    }

    data class Bucket<T : BucketNode>(val f: Double, val g: Double, val h: Double,
                                      val nodes: ArrayList<T>) : Indexable {

        override var index: Int = -1

        override fun toString(): String {
            val stringBuilder = StringBuilder()
            stringBuilder.appendln("---\n")
            stringBuilder.appendln("f: $f | g: $g | h: $h")
            stringBuilder.appendln("BucketNodeArray ${nodes.size}")
            nodes.forEach { stringBuilder.appendln(it.toString()) }
            stringBuilder.appendln("---")
            return stringBuilder.toString()
        }
    }

    fun getBucket(element: T): Bucket<T>? {
        val checkGHPair = GHPair(element.getGValue(), element.getHValue())
        return lookUpTable[checkGHPair]
    }

    val minFValue
        get() = fMin

    val numberOfBuckets
        get() = lookUpTable.size

    val size
        get() = openList.size

    fun isNotEmpty(): Boolean = size != 0

    fun add(element: T) = insert(element)

    fun chooseNode(): T? = pop()

    override fun toString(): String {
        val stringBuilder = StringBuilder()
                .appendln("fMin: $fMin")
                .appendln("OpenList size: ${openList.size}")

        openList.forEach { stringBuilder.append(it.toString()) }

        stringBuilder.appendln("---")
                .appendln("BucketLookUp size: ${lookUpTable.size}")

        lookUpTable.forEach { stringBuilder.appendln(it.value.toString()) }

        return stringBuilder.toString()
    }

    fun replace(element: T, replacement: T) {
        val elementGHPair = GHPair(element.getGValue(), element.getHValue())
        val bucketLookUp = lookUpTable[elementGHPair] ?: throw BucketOpenListException("Can't replace element. Element [$element] not found! ")

        bucketLookUp.nodes.remove(element)
        element.updateIndex(-1)

        if (bucketLookUp.nodes.isEmpty()) {
            openList.remove(elementGHPair)
        }

        if (element.getFValue() == minFValue) {
            recomputeMinFValue() // recompute the new minimum f value on open
            openList.reorder(PotentialComparator(bound, fMin)) // resort open list with new minimum f
        }

        insert(replacement)
    }

    private fun recomputeMinFValue() {
        if (openList.isNotEmpty()) {
            fMin = openList.peek()!!.f
            openList.forEach { bucket ->
                if (bucket.f < fMin) {
                    fMin = bucket.f
                }
            }
        } else {
            fMin = Double.MAX_VALUE
        }
    }

    private fun insert(element: T) {
        if (element.getFValue() < this.fMin) {
            fMin = element.getFValue()
            openList.reorder(PotentialComparator(bound, fMin))
        }

        val ghPair = Pair(element.getGValue(), element.getHValue())
        val elementGHPair = pairLookUp[ghPair]

        if (elementGHPair != null) {
            // have we seen this pair before
            val targetBucket = lookUpTable[elementGHPair]
            addElementToTargetBucket(element, targetBucket, elementGHPair)
        } else {
            // we have not seen this pair before make one
            val newGHPair = GHPair(element.getGValue(), element.getHValue())
            pairLookUp[ghPair] = newGHPair
            val targetBucket = lookUpTable[newGHPair]
            addElementToTargetBucket(element, targetBucket, newGHPair)
        }
    }

    private fun addElementToTargetBucket(element: T, targetBucket: Bucket<T>?, elementGHPair: GHPair) {
        if (targetBucket == null) {
            // make new bucket
            val bucketNodes = arrayListOf(element)
            val newBucket = Bucket(element.getFValue(), element.getGValue(), element.getHValue(), bucketNodes)

            element.updateIndex(bucketNodes.indexOf(element))
            openList.add(elementGHPair)
            lookUpTable[elementGHPair] = newBucket
        } else {
            // we have a bucket go get it
            val bucketNodes = targetBucket.nodes

            bucketNodes.add(element)
            element.updateIndex(bucketNodes.indexOf(element))
            if (bucketNodes.size == 1) openList.add(elementGHPair)
        }
    }

    private fun pop(): T? {
        if (size == 0) {
            throw BucketOpenListException("Nothing left to pop!")
        }

        val topBucketOnOpen = lookUpTable[openList.peek()]
        val firstElementInTopBucket = topBucketOnOpen?.nodes?.first() ?: throw BucketOpenListException("Nothing to pop!")

        topBucketOnOpen.nodes.remove(firstElementInTopBucket)

        if (topBucketOnOpen.nodes.isEmpty()) {
            openList.pop() // pop the empty bucket
        }

        if (firstElementInTopBucket.getFValue() == minFValue) {
            recomputeMinFValue() // recompute the new minimum f value on open
            openList.reorder(PotentialComparator(bound, fMin)) // resort open list with new minimum f
        }

        firstElementInTopBucket.updateIndex(-1)
        return firstElementInTopBucket
    }

}