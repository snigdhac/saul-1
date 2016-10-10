/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saul.infer

import java.io.PrintStream

import edu.illinois.cs.cogcomp.lbjava.classify.{ FeatureVector, ScoreSet }
import edu.illinois.cs.cogcomp.lbjava.learn.Learner
import edu.illinois.cs.cogcomp.saul.classifier.{ ConstrainedProblem, SaulConstraint }
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
import org.scalatest.{ Matchers, FlatSpec }

class StaticClassifier(trueLabelScore: Double) extends Learner("DummyClassifer") {
  override def getInputType: String = { "DummyInstance" }

  override def allowableValues: Array[String] = { Array[String]("false", "true") }

  override def equals(o: Any): Boolean = { getClass == o.getClass }

  /** The reason for true to be -1 is because the internal optimization by default finds the maximizer, while in this
    * problem we are looking for a minimizer
    */
  override def scores(example: AnyRef): ScoreSet = {
    val result: ScoreSet = new ScoreSet
    result.put("false", 0)
    result.put("true", trueLabelScore)
    result
  }

  override def write(printStream: PrintStream): Unit = ???

  override def scores(ints: Array[Int], doubles: Array[Double]): ScoreSet = ???

  override def classify(ints: Array[Int], doubles: Array[Double]): FeatureVector = ???

  override def learn(ints: Array[Int], doubles: Array[Double], ints1: Array[Int], doubles1: Array[Double]): Unit = ???
}

case class Instance(value: Int)

object DummyDataModel extends DataModel {
  val instances = node[Instance]

  /** definition of the constraints */
  val classifierPositiveScoreForTrue: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier(1.0)
  }
  val classifierNegativeScoreForTrue: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier(-1.0)
  }

  import SaulConstraint._

  def forAllTrue = instances.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isTrue2 }
  def forAllFalse = instances.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def existsTrue = instances.Exists { x: Instance => classifierNegativeScoreForTrue on2 x isTrue2 }
  def existsFalse = instances.Exists { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def atLeastTrue(k: Int) = instances.AtLeast(k) { x: Instance => classifierNegativeScoreForTrue on2 x isTrue2 }
  def atLeastFalse(k: Int) = instances.AtLeast(k) { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def atMostTrue(k: Int) = instances.AtMost(k) { x: Instance => classifierPositiveScoreForTrue on2 x isTrue2 }
  def atMostFalse(k: Int) = instances.AtMost(k) { x: Instance => classifierNegativeScoreForTrue on2 x isFalse2 }

  // negation
  def forAllTrueNegated = !forAllTrue
  def atLeastFalseNegated(k: Int) = !atLeastFalse(k)

  // conjunction
  def allTrueAllTrueConjunction = forAllTrue and4 forAllTrue
  def allTrueAllFalseConjunction = forAllTrue and4 forAllFalse
  def allFalseAllTrueConjunction = forAllFalse and4 forAllTrue
  def allFalseAllFalseConjunction = forAllFalse and4 forAllFalse

  // disjunction
  def allTrueAllTrueDisjunction = forAllTrue or4 forAllTrue
  def allTrueAllFalseDisjunction = forAllTrue or4 forAllFalse
  def allFalseAllTrueDisjunction = forAllFalse or4 forAllTrue
  def allFalseAllFalseDisjunction = forAllFalse or4 forAllFalse
}

class DummyConstrainedInference(someConstraint: Some[SaulConstraint[Instance]], classifier: LBJLearnerEquivalent) extends ConstrainedProblem[Instance, Instance] {
  override lazy val estimator = classifier
  override def pathToHead = None
  override def constraintsOpt = someConstraint
  override def solverType = OJAlgo
}

// TODO: negation, conjunction, disjunction, implication

class inferenceTest extends FlatSpec with Matchers {
  import DummyDataModel._

  val instances = (1 to 2).map(Instance)
  DummyDataModel.instances.populate(instances)
/*
  // all true
  "ForAll " should " return all true instances" in {
    val allTrueInference = new DummyConstrainedInference(Some(forAllTrue), classifierPositiveScoreForTrue)
    instances.foreach { ins => allTrueInference.build(ins) should be("true") }
  }

  // all false
  "ForAll " should " return all false instances" in {
    val allFalseInference = new DummyConstrainedInference(Some(forAllFalse), classifierPositiveScoreForTrue)
    instances.foreach { ins => allFalseInference.build(ins) should be("false") }
  }

  // exists true
  "Exists " should " return exactly one true when true weight is negative" in {
    val existOneTrueInference = new DummyConstrainedInference(Some(existsTrue), classifierNegativeScoreForTrue)
    instances.count { ins => existOneTrueInference.build(ins) == "true" } should be(1)
  }

  // exists false
  "Exists " should " return exactly one false when true weight is positive" in {
    val existOneFalseInference = new DummyConstrainedInference(Some(existsFalse), classifierPositiveScoreForTrue)
    instances.count { ins => existOneFalseInference.build(ins) == "false" } should be(1)
  }

  // at least 2 true
  "AtLeast " should " return at least two true instance" in {
    val atLeastTwoTrueInference = new DummyConstrainedInference(Some(atLeastTrue(2)), classifierNegativeScoreForTrue)
    instances.count { ins => atLeastTwoTrueInference.build(ins) == "true" } should be(2)
  }

  // at least 2 false
  "AtLeast " should " return at least two false instance" in {
    val atLeastTwoFalseInference = new DummyConstrainedInference(Some(atLeastFalse(2)), classifierPositiveScoreForTrue)
    instances.count { ins => atLeastTwoFalseInference.build(ins) == "false" } should be(2)
  }

  // at least 3 true
  "AtLeast " should " return at least three true instance" in {
    val atLeastThreeTrueInference = new DummyConstrainedInference(Some(atLeastTrue(3)), classifierNegativeScoreForTrue)
    instances.count { ins => atLeastThreeTrueInference.build(ins) == "true" } should be(3)
  }

  // at least 3 false
  "AtLeast " should " return at least three false instance" in {
    val atLeastThreeFalseInference = new DummyConstrainedInference(Some(atLeastFalse(3)), classifierPositiveScoreForTrue)
    instances.count { ins => atLeastThreeFalseInference.build(ins) == "false" } should be >= 3
  }
*/

  // at most 2 true
  "AtMost " should " return at most two true instances" in {
    val atMostTwoTrueInference = new DummyConstrainedInference(Some(atMostTrue(1)), classifierPositiveScoreForTrue)
    instances.count { ins => atMostTwoTrueInference.build(ins) == "true" } should be(1)
  }

//  // at most 2 false
//  "AtMost " should " return at most two false instances" in {
//    val atMostTwoFalseInference = new DummyConstrainedInference(Some(atMostFalse(1)), classifierNegativeScoreForTrue)
//    instances.count { ins => atMostTwoFalseInference.build(ins) == "false" } should be(1)
//  }
//
//  // at most 3 true
//  "AtMost " should " return at most three true instances" in {
//    val atMostThreeTrueInference = new DummyConstrainedInference(Some(atMostTrue(3)), classifierPositiveScoreForTrue)
//    instances.count { ins => atMostThreeTrueInference.build(ins) == "true" } should be(3)
//  }
//
//  // at most 3 false
//  "AtMost " should " return at most three false instances" in {
//    val atMostThreeFalseInference = new DummyConstrainedInference(Some(atMostFalse(3)), classifierNegativeScoreForTrue)
//    instances.count { ins => atMostThreeFalseInference.build(ins) == "false" } should be(3)
//  }

  // all true
//  "ForAll " should " return all true instances" in {
//    val allTrueInference = new DummyConstrainedInference(Some(forAllTrue), classifierPositiveScoreForTrue)
//    instances.foreach { ins => allTrueInference.build(ins) should be("true") }
//  }

  // negation of ForAllTrue
  //  "ForAllTrueNegated " should " contain at least one false" in {
  //    val forAllTrueNegatedInference = new DummyConstrainedInference(Some(forAllTrueNegated), classifierPositiveScoreForTrue)
  //    instances.exists { ins => forAllTrueNegatedInference.build(ins) == "false" } should be(true)
  //  }

  // negation of atLeastFalse(2): i.e. at most 8(=10-2) true
  //  "AtLeastFalse(2) " should " contain at least one false" in {
  //    val atLeastFalseNegatedInference = new DummyConstrainedInference(Some(atLeastFalseNegated(2)), classifierPositiveScoreForTrue)
  //    instances.count { ins => atLeastFalseNegatedInference.build(ins) == "false" } should be (8)
  //  }

  //    // conjunctions
  //      "AllTrueAllTrueConjunction " should " always be true" in {
  //        val allTrueAllTrueConjunctionInference = new DummyConstrainedInference(Some(allTrueAllTrueConjunction), classifierPositiveScoreForTrue)
  //        instances.forall { ins => allTrueAllTrueConjunctionInference.build(ins) == "true" } should be(true)
  //      }
  //
  //    "AllFalseAllTrueConjunction " should " always be false" in {
  //      val allFalseAllFalseConjunctionInference = new DummyConstrainedInference(Some(allFalseAllFalseConjunction))
  //      instances.forall { ins => allFalseAllFalseConjunctionInference.build(ins) == "false" } should be(true)
  //    }
  //
  //    "AllTrueAllFalseConjunction " should " always be infeasible" in {
  //      // TODO: how to test this?
  //    }
  //
  //    "AllFalseAllTrueConjunction " should " always be infeasible" in {
  //      // TODO: how to test this?
  //    }
  //
  //    // disjunctions
  //    "AllTrueAllTrueDisjunction " should " always be true" in {
  //      val allTrueAllTrueDisjunctionInference = new DummyConstrainedInference(Some(allTrueAllTrueDisjunction))
  //      instances.forall { ins => allTrueAllTrueDisjunctionInference.build(ins) == "true" } should be(true)
  //    }
  //
  //    "AllFalseAllFalseDisjunction " should " always be false" in {
  //      val allFalseAllFalseDisjunctionInference = new DummyConstrainedInference(Some(allFalseAllFalseDisjunction))
  //      instances.forall { ins => allFalseAllFalseDisjunctionInference.build(ins) == "false" } should be(true)
  //    }
  //
  //    "AllTrueAllFalseDisjunction " should " always all be false, or should all be true" in {
  //      val allTrueAllFalseDisjunctionInference = new DummyConstrainedInference(Some(allTrueAllFalseDisjunction))
  //      (instances.forall { ins => allTrueAllFalseDisjunctionInference.build(ins) == "false" } ||
  //        instances.forall { ins => allTrueAllFalseDisjunctionInference.build(ins) == "true" }) should be(true)
  //    }
  //
  //    "AllFalseAllTrueDisjunction " should " always all be false, or should all be true" in {
  //      val allFalseAllTrueDisjunctionInference = new DummyConstrainedInference(Some(allFalseAllTrueDisjunction))
  //      (instances.forall { ins => allFalseAllTrueDisjunctionInference.build(ins) == "false" } ||
  //        instances.forall { ins => allFalseAllTrueDisjunctionInference.build(ins) == "true" }) should be(true)
  //    }
}
