/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.setcover

import edu.illinois.cs.cogcomp.lbjava.learn.Learner
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedProblem
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
import edu.illinois.cs.cogcomp.saul.util.Logging

import scala.collection.JavaConversions._

object SetCoverApp extends Logging {
  val cityInstances = new City("src/main/resources/SetCover/example.txt")
  val neighborhoodInstances = cityInstances.getNeighborhoods.toList
  object cp extends ConstrainedProblem[Neighborhood, City] {
    override lazy val estimator = new LBJLearnerEquivalent {
      override val classifier: Learner = new ContainsStation()
    }
    override def pathToHead = Some(-SetCoverSolverDataModel2.cityContainsNeighborhoods)
    override def constraintsOpt = Some(SetCoverSolverDataModel2.containsStationConstraint2)
    override def solverType = OJAlgo
  }

  def main(args: Array[String]) {
    //    oldApt()
    newApp()
  }

  def newApp(): Unit = {
    println("in main: allowable values: " + new ContainsStation().allowableValues.toSeq)
    SetCoverSolverDataModel2.cities populate List(cityInstances)
    SetCoverSolverDataModel2.neighborhoods populate neighborhoodInstances
    def getParentCity = (n: Neighborhood) => n.getParentCity
    SetCoverSolverDataModel2.cityContainsNeighborhoods.populateWith((c: City, n: Neighborhood) => n.getParentCity == c)
    //cp.build()
    cityInstances.getNeighborhoods.foreach {
      n => logger.info(n.getNumber + ": " + cp.build(n))
    }
  }

  def oldApt(): Unit = {
    SetCoverSolverDataModel.cities populate List(cityInstances)
    SetCoverSolverDataModel.neighborhoods populate neighborhoodInstances
    SetCoverSolverDataModel.cityContainsNeighborhoods.populateWith(_ == _.getParentCity)

    /** printing the labels for each nrighborhood (whether they are choosen to be covered by a station, or not) */
    cityInstances.getNeighborhoods.foreach {
      n => logger.info(n.getNumber + ": " + ContainsStationConstraint(n))
    }
  }
}