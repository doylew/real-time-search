package edu.unh.cs.ai.realtimesearch.visualizer.gridbased

import edu.unh.cs.ai.realtimesearch.experiment.configuration.Configurations
import edu.unh.cs.ai.realtimesearch.util.convertNanoUpDouble
import edu.unh.cs.ai.realtimesearch.visualizer.ThemeColors
import javafx.animation.Interpolator
import javafx.animation.PathTransition
import javafx.animation.SequentialTransition
import javafx.animation.Timeline
import javafx.scene.shape.Circle
import javafx.scene.shape.LineTo
import javafx.scene.shape.MoveTo
import javafx.scene.shape.Path
import javafx.util.Duration
import java.util.concurrent.TimeUnit

/**
 * Created by Stephen on 2/29/16.
 */
class PointInertiaVisualizer : PointVisualizer() {
    private var xDot = 0.0
    private var yDot = 0.0
//    private var domain: PointRobotWithInertia? = null
//    private var environment: PointRobotWithInertiaEnvironment? = null

    override fun setupDomain() {
//        val numActions = (experimentResult!!.experimentConfiguration[Configurations.NUM_ACTIONS.toString()] as Long?)?.toInt() ?: PointRobotWithInertia.defaultNumActions
//        val stateFraction = experimentResult!!.experimentConfiguration[Configurations.STATE_FRACTION.toString()] as Double? ?: PointRobotWithInertia.defaultStateFraction
//        val actionFraction = experimentResult!!.experimentConfiguration[Configurations.ACTION_FRACTION.toString()] as Double? ?: PointRobotWithInertia.defaultActionFraction
//        domain = PointRobotWithInertia(
//                mapInfo.columnCount,
//                mapInfo.rowCount,
//                mapInfo.blockedCells.toHashSet(),
//                mapInfo.goalCells.first().toDoubleLocation(),
//                header!!.goalRadius,
//                numActions,
//                actionFraction,
//                stateFraction,
//                actionDuration
//        )
//        environment = PointRobotWithInertiaEnvironment(domain!!, PointRobotWithInertiaState(startX, startY, 0.0, 0.0, actionFraction))
    }

    override fun playAnimation(transitions: List<PathTransition>) {
        val sequentialTransition = SequentialTransition()
        for (pathTransition in transitions) {
            sequentialTransition.children.add(pathTransition)
        }
        sequentialTransition.cycleCount = Timeline.INDEFINITE

        Thread({
            val delayTime = convertNanoUpDouble(experimentResult.idlePlanningTime, TimeUnit.MILLISECONDS) * animationTime / convertNanoUpDouble(experimentResult.experimentConfiguration[Configurations.ACTION_DURATION.toString()] as Long, TimeUnit.MILLISECONDS)
            println("Delay:  $delayTime")
            Thread.sleep(delayTime.toLong())
            sequentialTransition.play()
        }).start()
    }

    override fun buildAnimation(): List<PathTransition> {
        /* Create the path that the robot will travel */
        if (displayLine) {
            val path = Path()
            path.elements.add(MoveTo(agentView.agent.x, agentView.agent.y))
            path.stroke = ThemeColors.PATH.stroke
            grid.children.add(path)
        }

        val pathTransitions = mutableListOf<PathTransition>()
        val actionIterator = actionList.iterator()
        while (actionIterator.hasNext()) {
            val x = actionIterator.next()
            assert(actionIterator.hasNext(), { "Action has no matching y value" })
            val y = actionIterator.next()
            var pathTransition = animate(x, y)
            pathTransitions.addAll(pathTransition)
        }

        return pathTransitions
    }

    override fun animate(x: String, y: String): MutableList<PathTransition> {
        val robot = agentView.agent
        val width = tileSize
        val retval: MutableList<PathTransition> = arrayListOf()

        val xDDot = x.toDouble() * width
        val yDDot = y.toDouble() * width

        val nSteps = 100
        val dt = 1.0 / nSteps

        for (i in 0..nSteps-1) {
            val path = Path()
            path.elements.add(MoveTo(animationX, animationY))

            var xdot = xDot + xDDot * (dt * i)
            var ydot = yDot + yDDot * (dt * i)

            path.elements.add(LineTo(animationX + (xdot * dt), animationY + (ydot * dt)))
            animationX += xdot * dt
            animationY += ydot * dt

            if(displayLine){
                path.stroke = ThemeColors.PATH.stroke
                grid.children.add(path)
            }

            /* Animate the robot */
            val pathTransition = PathTransition()
            pathTransition.duration = Duration.millis(10.0)
            pathTransition.path = path
            pathTransition.node = robot
            pathTransition.interpolator = Interpolator.LINEAR
            retval.add(pathTransition)
        }

        xDot += xDDot
        yDot += yDDot

        if(displayLine){
            val action = Circle(animationX, animationY, width / 10.0)
            grid.children.add(action)
        }
        return retval
    }
}
