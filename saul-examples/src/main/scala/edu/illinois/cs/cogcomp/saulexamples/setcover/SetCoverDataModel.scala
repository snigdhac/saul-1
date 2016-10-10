/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.setcover

import edu.illinois.cs.cogcomp.lbjava.learn.Learner
import edu.illinois.cs.cogcomp.saul.classifier.{ ConstrainedProblem, SaulConstraint }
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent

object SetCoverSolverDataModel extends DataModel {

  val cities = node[City]

  val neighborhoods = node[Neighborhood]

  val cityContainsNeighborhoods = edge(cities, neighborhoods)

  cityContainsNeighborhoods.populateWith((c, n) => c == n.getParentCity)

  import SaulConstraint._

  /** definition of the constraints */
  val containStation: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new ContainsStation()
  }

  def atLeastANeighborOfNeighborhoodIsCovered = { n: Neighborhood =>
    n.getNeighbors.Exists { neighbor: Neighborhood => containStation on2 neighbor isTrue2 }
  }

  def neighborhoodContainsStation = { n: Neighborhood =>
    containStation on2 n isTrue2
  }

  def allCityNeiborhoodsAreCovered = { x: City =>
    x.getNeighborhoods.ForAll { n: Neighborhood =>
      neighborhoodContainsStation(n) or4 atLeastANeighborOfNeighborhoodIsCovered(n)
    }
  }

  def containsStationConstraint = SetCoverSolverDataModel.cities.ForAll { x: City => allCityNeiborhoodsAreCovered(x) }
}

object ConstrainedContainsStation extends ConstrainedProblem[Neighborhood, City] {
  override lazy val estimator = SetCoverSolverDataModel.containStation
  override def pathToHead = Some(-SetCoverSolverDataModel.cityContainsNeighborhoods)
  override def constraintsOpt = Some(SetCoverSolverDataModel.containsStationConstraint)
  override def solverType = OJAlgo
}
