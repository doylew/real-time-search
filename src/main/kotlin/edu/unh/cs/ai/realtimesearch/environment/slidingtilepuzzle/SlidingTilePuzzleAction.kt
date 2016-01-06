package edu.unh.cs.ai.realtimesearch.environment.slidingtilepuzzle

import edu.unh.cs.ai.realtimesearch.environment.Action

/**
 * @author Bence Cserna (bence@cserna.net)
 */
enum class SlidingTilePuzzleAction(val index: Int) : Action {
    NORTH(0), SOUTH(1), WEST(2), EAST(3);

    // Storage of all relative locations (up down left right), returned by reference
    private val relativeLocations = arrayOf(
            SlidingTilePuzzleState.Location(0, 1),
            SlidingTilePuzzleState.Location(0, -1),
            SlidingTilePuzzleState.Location(-1, 0),
            SlidingTilePuzzleState.Location(1, 0)
    )

    fun getRelativeLocation() = relativeLocations[index]

}