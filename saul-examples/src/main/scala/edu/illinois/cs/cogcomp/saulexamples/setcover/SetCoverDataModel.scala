/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.setcover

import edu.illinois.cs.cogcomp.infer.ilp.OJalgoHook
import edu.illinois.cs.cogcomp.lbjava.learn.Learner
import edu.illinois.cs.cogcomp.saul.classifier.{ SaulConstraint, ConstrainedClassifier }
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.constraint.ConstraintTypeConversion._
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent

object SetCoverSolverDataModel extends DataModel {

  val cities = node[City]

  val neighborhoods = node[Neighborhood]

  val cityContainsNeighborhoods = edge(cities, neighborhoods)

  cityContainsNeighborhoods.populateWith((c, n) => c == n.getParentCity)

  /** definition of the constraints */
  val containStation = new ContainsStation()

  def atLeastANeighborOfNeighborhoodIsCovered = { n: Neighborhood =>
    n.getNeighbors._exists { neighbor: Neighborhood => containStation on neighbor isTrue }
  }

  def neighborhoodContainsStation = { n: Neighborhood =>
    containStation on n isTrue
  }

  def allCityNeiborhoodsAreCovered = { x: City =>
    x.getNeighborhoods._forall { n: Neighborhood =>
      neighborhoodContainsStation(n) or atLeastANeighborOfNeighborhoodIsCovered(n)
    }
  }

  def someCityNeiborhoodsAreCovered = { x: City =>
    x.getNeighborhoods._atleast(2) { n: Neighborhood =>
      neighborhoodContainsStation(n) //or atLeastANeighborOfNeighborhoodIsCovered(n)
    }
  }

  val containsStationConstraint = ConstrainedClassifier.constraint[City] { x: City => allCityNeiborhoodsAreCovered(x) }
}

object SetCoverSolverDataModel2 extends DataModel {

  val cities = node[City]

  val neighborhoods = node[Neighborhood]

  val cityContainsNeighborhoods = edge(cities, neighborhoods)

  cityContainsNeighborhoods.populateWith((c, n) => c == n.getParentCity)

  /** definition of the constraints */
  val containStation: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new ContainsStation()
  }

  import SaulConstraint._

  def atLeastANeighborOfNeighborhoodIsCovered2 = { n: Neighborhood =>
    n.getNeighbors.Exists { neighbor: Neighborhood => containStation on2 neighbor isTrue2 }
  }

  def neighborhoodContainsStation2 = { n: Neighborhood =>
    containStation on2 n isTrue2
  }

  def fancyConstraint = { n: Neighborhood =>
    (containStation on2 n isTrue2) and4 (containStation on2 n isTrue2)
  }

  def fancyConstraint2 = { n: Neighborhood =>
    (containStation on2 n isTrue2) or4 (containStation on2 n isTrue2)
  }

  def fancyConstraint3 = { n: Neighborhood =>
    (containStation on2 n isTrue2) or4 (containStation on2 n isTrue2) and4 (containStation on2 n isTrue2)
  }

  val x: List[City] = ???

  def allCityNeiborhoodsAreCovered = { x: City =>
    x.getNeighborhoods.ForAll { n: Neighborhood =>
      neighborhoodContainsStation2(n).or4(atLeastANeighborOfNeighborhoodIsCovered2(n))
    }
  }

  val containsStationConstraint2 = SetCoverSolverDataModel2.cities.ForAll { x: City => allCityNeiborhoodsAreCovered(x) }
}

import SetCoverSolverDataModel._
object ContainsStationConstraint extends ConstrainedClassifier[Neighborhood, City](new ContainsStation()) {
  override val pathToHead = Some(-cityContainsNeighborhoods)
  override def subjectTo = containsStationConstraint
  override val solver = new OJalgoHook
}
