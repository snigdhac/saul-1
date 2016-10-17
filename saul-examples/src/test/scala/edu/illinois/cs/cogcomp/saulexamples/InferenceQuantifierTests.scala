/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
///** This software is released under the University of Illinois/Research and Academic Use License. See
//  * the LICENSE file in the root folder for details. Copyright (c) 2016
//  *
//  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
//  * http://cogcomp.cs.illinois.edu/
//  */
//package edu.illinois.cs.cogcomp.saulexamples
//
//import edu.illinois.cs.cogcomp.lbjava.learn.Learner
//import edu.illinois.cs.cogcomp.saul.classifier.{ ConstrainedProblem, SaulConstraint }
//import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
//import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
//import edu.illinois.cs.cogcomp.saulexamples.setcover.{ City, ContainsStation, Neighborhood }
//import org.scalatest.{ Matchers, FlatSpec }
//
//import SaulConstraint._
//import scala.collection.JavaConversions._
//
//class InferenceQuantifierTests extends FlatSpec with Matchers {
//
//  object SomeDM extends DataModel {
//
//    val cities = node[City]
//
//    val neighborhoods = node[Neighborhood]
//
//    val cityContainsNeighborhoods = edge(cities, neighborhoods)
//
//    cityContainsNeighborhoods.populateWith((c, n) => c == n.getParentCity)
//
//    /** definition of the constraints */
//    val containStation = new ContainsStation()
//
//    val containStationLBJEquivalent: LBJLearnerEquivalent = new LBJLearnerEquivalent {
//      override val classifier: Learner = new ContainsStation()
//    }
//
//    def neighborhoodContainsStation(n: Neighborhood) = containStationLBJEquivalent on2 n isTrue2
//
//    val atLeastSomeNeighborsAreCoveredConstraint = cities.ForAll { x: City =>
//      x.getNeighborhoods.AtLeast(2) { n: Neighborhood => neighborhoodContainsStation(n) }
//    }
//
//    val atLeastSomeNeighborsAreCoveredConstraintUsingAtMost = cities.ForAll { x: City =>
//      !x.getNeighborhoods.AtMost(2) { n: Neighborhood => neighborhoodContainsStation(n) }
//    }
//
//    val allNeighborsAreCoveredConstraint = cities.ForAll { x: City =>
//      x.getNeighborhoods.ForAll { n: Neighborhood => neighborhoodContainsStation(n) }
//    }
//
//    val singleNeighborsAreCoveredConstraint = cities.ForAll { x: City =>
//      x.getNeighborhoods.Exists { n: Neighborhood => neighborhoodContainsStation(n) }
//    }
//  }
//
//  import SomeDM._
//  object AtLeastSomeNeighborhoods extends ConstrainedProblem[Neighborhood, City] {
//    override val pathToHead = Some(-cityContainsNeighborhoods)
//    override def constraintsOpt = Some(atLeastSomeNeighborsAreCoveredConstraint)
//    override val solverType = OJAlgo
//    override def estimator: LBJLearnerEquivalent = containStationLBJEquivalent
//  }
//
//  object AtLeastSomeNeighborhoodsUsingAtMost extends ConstrainedProblem[Neighborhood, City] {
//    override val pathToHead = Some(-cityContainsNeighborhoods)
//    override def constraintsOpt = Some(atLeastSomeNeighborsAreCoveredConstraintUsingAtMost)
//    override val solverType = OJAlgo
//    override def estimator: LBJLearnerEquivalent = containStationLBJEquivalent
//  }
//
//  object AllNeighborhoods extends ConstrainedProblem[Neighborhood, City] {
//    override val pathToHead = Some(-cityContainsNeighborhoods)
//    override def constraintsOpt = Some(allNeighborsAreCoveredConstraint)
//    override val solverType = OJAlgo
//    override def estimator: LBJLearnerEquivalent = containStationLBJEquivalent
//  }
//
//  object ASingleNeighborhood extends ConstrainedProblem[Neighborhood, City] {
//    override val pathToHead = Some(-cityContainsNeighborhoods)
//    override def constraintsOpt = Some(singleNeighborsAreCoveredConstraint)
//    override val solverType = OJAlgo
//    override def estimator: LBJLearnerEquivalent = containStationLBJEquivalent
//  }
//
//  val cityInstances = new City("../saul-examples/src/test/resources/SetCover/example.txt")
//  val neighborhoodInstances = cityInstances.getNeighborhoods.toList
//
//  SomeDM.cities populate List(cityInstances)
//  SomeDM.neighborhoods populate neighborhoodInstances
//  SomeDM.cityContainsNeighborhoods.populateWith(_ == _.getParentCity)
//
//  "Quantifier atleast " should " work " in {
//    cityInstances.getNeighborhoods.count(n => AtLeastSomeNeighborhoods(n) == "true") should be(2)
//  }
//
//  // negation of atmost(2) is equivalent to atleast(2)
//  "Quantifier atmost " should " work " in {
//    cityInstances.getNeighborhoods.count(n => AtLeastSomeNeighborhoodsUsingAtMost(n) == "true") should be(3)
//  }
//
//  "Quantifier forall " should " work " in {
//    cityInstances.getNeighborhoods.count(n => AllNeighborhoods(n) == "true") should be(9)
//  }
//
//  "Quantifier exists " should " work " in {
//    cityInstances.getNeighborhoods.count(n => ASingleNeighborhood(n) == "true") should be(1)
//  }
//}
