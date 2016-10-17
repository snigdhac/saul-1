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

import SaulConstraint._

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
  val instanceNode = node[Instance]

  // only used for implications
  val instanceNode2 = node[Instance]

  /** definition of the constraints */
  val classifierPositiveScoreForTrue: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier(1.0)
  }
  val classifierNegativeScoreForTrue: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier(-1.0)
  }

  def singleInstanceMustBeTrue(x: Instance) = { classifierNegativeScoreForTrue on2 x isTrue2 }
  def singleInstanceMustBeFalse(x: Instance) = { classifierPositiveScoreForTrue on2 x isFalse2 }
  def forAllTrue = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isTrue2 }
  def forAllFalse = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def forAllOneOfTheLabelsPositiveClassifier = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isOneOf ("true", "true") }
  def forAllOneOfTheLabelsNegativeClassifier = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isOneOf ("true", "true") }
  def forAllNotFalse = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isNot2 "false" }
  def forAllNotTrue = instanceNode.ForAll { x: Instance => classifierPositiveScoreForTrue on2 x isNot2 "true" }
  def existsTrue = instanceNode.Exists { x: Instance => classifierNegativeScoreForTrue on2 x isTrue2 }
  def existsFalse = instanceNode.Exists { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def exatclyTrue(k: Int) = instanceNode.Exactly(k) { x: Instance => classifierPositiveScoreForTrue on2 x isTrue2 }
  def exatclyFalse(k: Int) = instanceNode.Exactly(k) { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def atLeastTrue(k: Int) = instanceNode.AtLeast(k) { x: Instance => classifierNegativeScoreForTrue on2 x isTrue2 }
  def atLeastFalse(k: Int) = instanceNode.AtLeast(k) { x: Instance => classifierPositiveScoreForTrue on2 x isFalse2 }
  def atMostTrue(k: Int) = instanceNode.AtMost(k) { x: Instance => classifierPositiveScoreForTrue on2 x isTrue2 }
  def atMostFalse(k: Int) = instanceNode.AtMost(k) { x: Instance => classifierNegativeScoreForTrue on2 x isFalse2 }
  def classifierHasSameValueOnTwoInstances(x: Instance, y: Instance) = classifierPositiveScoreForTrue on4 x equalsTo y

  // negation
  def forAllFalseWithNegation = instanceNode.ForAll { x: Instance => !(classifierPositiveScoreForTrue on2 x isTrue2) }
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

// TODO:
// implication,

class inferenceTest extends FlatSpec with Matchers {
  import DummyDataModel._

  val instanceSet = (1 to 5).map(Instance)
  DummyDataModel.instanceNode.populate(instanceSet)

  // extra constraints based on data
  // all instances should have the same label
  val classifierHasSameValueOnTwoInstancesInstantiated = {
    classifierHasSameValueOnTwoInstances(instanceSet(0), instanceSet(1)) and4
      classifierHasSameValueOnTwoInstances(instanceSet(1), instanceSet(2)) and4
      classifierHasSameValueOnTwoInstances(instanceSet(2), instanceSet(3)) and4
      classifierHasSameValueOnTwoInstances(instanceSet(3), instanceSet(4))
  }

  val allInstancesShouldBeTrue = {
    classifierHasSameValueOnTwoInstancesInstantiated and4 singleInstanceMustBeTrue(instanceSet(0))
  }

  val trueImpliesTrue = {
    ((classifierNegativeScoreForTrue on2 instanceSet(0) isTrue2) ====>
      (classifierNegativeScoreForTrue on2 instanceSet(1) isTrue2)) and4 (classifierNegativeScoreForTrue on2 instanceSet(0) isTrue2)
  }

  val trueImpliesFalse = {
    ((classifierNegativeScoreForTrue on2 instanceSet(0) isTrue2) ====>
      (classifierNegativeScoreForTrue on2 instanceSet(1) isFalse2)) and4 (classifierNegativeScoreForTrue on2 instanceSet(0) isTrue2)
  }

  val falseImpliesTrue = {
    ((classifierNegativeScoreForTrue on2 instanceSet(0) isFalse2) ====>
      (classifierNegativeScoreForTrue on2 instanceSet(1) isTrue2)) and4 (classifierNegativeScoreForTrue on2 instanceSet(0) isFalse2)
  }

  val falseImpliesFalse = {
    ((classifierNegativeScoreForTrue on2 instanceSet(0) isFalse2) ====>
      (classifierNegativeScoreForTrue on2 instanceSet(1) isFalse2)) and4 (classifierNegativeScoreForTrue on2 instanceSet(0) isFalse2)
  }

  // single instance constraint
  "first instance " should "true and the rest should be false" in {
    val singleInstanceMustBeTrueInference = new DummyConstrainedInference(
      Some(singleInstanceMustBeTrue(instanceSet.head)), classifierNegativeScoreForTrue
    )
    singleInstanceMustBeTrueInference.build(instanceSet.head) should be("true")
    instanceSet.drop(1).foreach { ins => singleInstanceMustBeTrueInference.build(ins) should be("false") }
  }

  // single instance constraint
  "first instance " should "false and the rest should be true" in {
    val singleInstanceMustBeFalseInference = new DummyConstrainedInference(
      Some(singleInstanceMustBeFalse(instanceSet.head)), classifierPositiveScoreForTrue
    )
    singleInstanceMustBeFalseInference.build(instanceSet.head) should be("false")
    instanceSet.drop(1).foreach { ins => singleInstanceMustBeFalseInference.build(ins) should be("true") }
  }

  // all true
  "ForAllTrue " should " return all true instances" in {
    val allTrueInference = new DummyConstrainedInference(Some(forAllTrue), classifierPositiveScoreForTrue)
    instanceSet.foreach { ins => allTrueInference.build(ins) should be("true") }
  }

  // all false
  "ForAllFalse " should " return all false instances" in {
    val allFalseInference = new DummyConstrainedInference(Some(forAllFalse), classifierPositiveScoreForTrue)
    instanceSet.foreach { ins => allFalseInference.build(ins) should be("false") }
  }

  // for all one of the labels
  "OneOf(true, some label) with positive true weight " should " work properly " in {
    val forAllOneOfTheLabelsPositiveClassifierInference = new DummyConstrainedInference(
      Some(forAllOneOfTheLabelsPositiveClassifier), classifierPositiveScoreForTrue
    )
    instanceSet.foreach { ins => forAllOneOfTheLabelsPositiveClassifierInference.build(ins) should be("true") }
  }

  // for all one of the labels
  "OneOf(true, some label) with negative true weight " should " work properly " in {
    val forAllOneOfTheLabelsNegativeClassifierInference = new DummyConstrainedInference(
      Some(forAllOneOfTheLabelsNegativeClassifier), classifierPositiveScoreForTrue
    )
    instanceSet.foreach { ins => forAllOneOfTheLabelsNegativeClassifierInference.build(ins) should be("true") }
  }

  // all not false, should always return true
  "ForAllNotFalse " should " return all true instances" in {
    val allNotFalseInference = new DummyConstrainedInference(Some(forAllNotFalse), classifierPositiveScoreForTrue)
    instanceSet.foreach { ins => allNotFalseInference.build(ins) should be("true") }
  }

  // all not true, should always return false
  "ForAllNotTrue " should " return all false instances" in {
    val allNotTrueInference = new DummyConstrainedInference(Some(forAllNotTrue), classifierPositiveScoreForTrue)
    instanceSet.foreach { ins => allNotTrueInference.build(ins) should be("false") }
    instanceSet.foreach { ins => info(allNotTrueInference.build(ins)) }
  }

  // exists true
  "ExistsTrue " should " return exactly one true when true weight is negative" in {
    val existOneTrueInference = new DummyConstrainedInference(Some(existsTrue), classifierNegativeScoreForTrue)
    instanceSet.count { ins => existOneTrueInference.build(ins) == "true" } should be(1)
  }

  // exists false
  "ExistsFalse " should " return exactly one false when true weight is positive" in {
    val existOneFalseInference = new DummyConstrainedInference(Some(existsFalse), classifierPositiveScoreForTrue)
    instanceSet.count { ins => existOneFalseInference.build(ins) == "false" } should be(1)
  }

  // at least 2 true
  "AtLeast2True " should " return at least two true instance" in {
    val atLeastTwoTrueInference = new DummyConstrainedInference(Some(atLeastTrue(2)), classifierNegativeScoreForTrue)
    instanceSet.count { ins => atLeastTwoTrueInference.build(ins) == "true" } should be(2)
  }

  // at least 2 false
  "AtLeast2False " should " return at least two false instance" in {
    val atLeastTwoFalseInference = new DummyConstrainedInference(Some(atLeastFalse(2)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => atLeastTwoFalseInference.build(ins) == "false" } should be(2)
  }

  // at least 3 true
  "AtLeast3True " should " return at least three true instance" in {
    val atLeastThreeTrueInference = new DummyConstrainedInference(Some(atLeastTrue(3)), classifierNegativeScoreForTrue)
    instanceSet.count { ins => atLeastThreeTrueInference.build(ins) == "true" } should be(3)
  }

  // at least 3 false
  "AtLeast3False " should " return at least three false instance" in {
    val atLeastThreeFalseInference = new DummyConstrainedInference(Some(atLeastFalse(3)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => atLeastThreeFalseInference.build(ins) == "false" } should be >= 3
  }

  // exactly 1 true
  "ExactlyOneTrue " should " return exactly one true instance" in {
    val exactlyOneTrue = new DummyConstrainedInference(Some(exatclyTrue(1)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneTrue.build(ins) == "true" } should be(1)
  }

  // exactly 2 true
  "ExactlyTwoTrue " should " return exactly two true instances" in {
    val exactlyOneTrue = new DummyConstrainedInference(Some(exatclyTrue(2)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneTrue.build(ins) == "true" } should be(2)
  }

  // exactly 3 true
  "ExactlyTwoTrue " should " return exactly three true instances" in {
    val exactlyOneTrue = new DummyConstrainedInference(Some(exatclyTrue(3)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneTrue.build(ins) == "true" } should be(3)
  }

  // exactly 1 false
  "ExactlyOneFalse " should " return exactly one true instances" in {
    val exactlyOneFalse = new DummyConstrainedInference(Some(exatclyFalse(1)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneFalse.build(ins) == "false" } should be(1)
  }

  // exactly 2 false
  "ExactlyTwoFalse " should " return exactly two true instances" in {
    val exactlyOneFalse = new DummyConstrainedInference(Some(exatclyFalse(2)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneFalse.build(ins) == "false" } should be(2)
  }

  // exactly 3 false
  "ExactlyTwoFalse " should " return exactly three true instances" in {
    val exactlyOneFalse = new DummyConstrainedInference(Some(exatclyFalse(3)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => exactlyOneFalse.build(ins) == "false" } should be(3)
  }

  // at most 2 true
  "AtMost " should " return at most two true instances" in {
    val atMostTwoTrueInference = new DummyConstrainedInference(Some(atMostTrue(1)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => atMostTwoTrueInference.build(ins) == "true" } should be(1)
  }

  // at most 2 false
  "AtMost " should " return at most two false instances" in {
    val atMostTwoFalseInference = new DummyConstrainedInference(Some(atMostFalse(1)), classifierNegativeScoreForTrue)
    instanceSet.count { ins => atMostTwoFalseInference.build(ins) == "false" } should be(1)
  }

  // at most 3 true
  "AtMost " should " return at most three true instances" in {
    val atMostThreeTrueInference = new DummyConstrainedInference(Some(atMostTrue(3)), classifierPositiveScoreForTrue)
    instanceSet.count { ins => atMostThreeTrueInference.build(ins) == "true" } should be(3)
  }

  // at most 3 false
  "AtMost " should " return at most three false instances" in {
    val atMostThreeFalseInference = new DummyConstrainedInference(Some(atMostFalse(3)), classifierNegativeScoreForTrue)
    instanceSet.count { ins => atMostThreeFalseInference.build(ins) == "false" } should be(3)
  }

  // negation of ForAllTrue
  "ForAllFalseWithNegation " should " all be false" in {
    val forAllFalseWithNegationInference = new DummyConstrainedInference(Some(forAllFalseWithNegation), classifierPositiveScoreForTrue)
    instanceSet.count { ins => forAllFalseWithNegationInference.build(ins) == "false" } should be(instanceSet.length)
  }

  // negation of ForAllTrue
  "ForAllTrueNegated " should " contain at least one false" in {
    val forAllTrueNegatedInference = new DummyConstrainedInference(Some(forAllTrueNegated), classifierPositiveScoreForTrue)
    instanceSet.count { ins => forAllTrueNegatedInference.build(ins) == "false" } should be >= 1
  }

  // conjunctions
  "AllTrueAllTrueConjunction " should " always be true" in {
    val allTrueAllTrueConjunctionInference = new DummyConstrainedInference(Some(allTrueAllTrueConjunction), classifierPositiveScoreForTrue)
    instanceSet.forall { ins => allTrueAllTrueConjunctionInference.build(ins) == "true" } should be(true)
  }

  "AllFalseAllTrueConjunction " should " always be false" in {
    val allFalseAllFalseConjunctionInference = new DummyConstrainedInference(Some(allFalseAllFalseConjunction), classifierPositiveScoreForTrue)
    instanceSet.forall { ins => allFalseAllFalseConjunctionInference.build(ins) == "false" } should be(true)
  }

  "AllTrueAllFalseConjunction " should " always be infeasible" in {
    //        val alltrueAlltrueConjunctionInference = new DummyConstrainedInference(Some(allTrueAllTrueConjunction),
    //          classifierPositiveScoreForTrue)
    //        instances.forall { ins => alltrueAlltrueConjunctionInference.build(ins) == "false" } should be(true)
    // TODO: how to test this?
  }

  "AllFalseAllTrueConjunction " should " always be infeasible" in {
    // TODO: how to test this?
  }

  // disjunctions
  "AllTrueAllTrueDisjunction " should " always be true" in {
    val allTrueAllTrueDisjunctionInference = new DummyConstrainedInference(Some(allTrueAllTrueDisjunction), classifierPositiveScoreForTrue)
    instanceSet.forall { ins => allTrueAllTrueDisjunctionInference.build(ins) == "true" } should be(true)
  }

  "AllFalseAllFalseDisjunction " should " always be false" in {
    val allFalseAllFalseDisjunctionInference = new DummyConstrainedInference(Some(allFalseAllFalseDisjunction), classifierPositiveScoreForTrue)
    instanceSet.count { ins => allFalseAllFalseDisjunctionInference.build(ins) == "false" } should be(instanceSet.size)
  }

  "AllTrueAllFalseDisjunction " should " always all be false, or should all be true" in {
    val allTrueAllFalseDisjunctionInference = new DummyConstrainedInference(Some(allTrueAllFalseDisjunction), classifierPositiveScoreForTrue)
    (instanceSet.forall { ins => allTrueAllFalseDisjunctionInference.build(ins) == "false" } ||
      instanceSet.forall { ins => allTrueAllFalseDisjunctionInference.build(ins) == "true" }) should be(true)
  }

  "AllFalseAllTrueDisjunction " should " always all be false, or should all be true" in {
    val allFalseAllTrueDisjunctionInference = new DummyConstrainedInference(Some(allFalseAllTrueDisjunction), classifierPositiveScoreForTrue)
    (instanceSet.forall { ins => allFalseAllTrueDisjunctionInference.build(ins) == "false" } ||
      instanceSet.forall { ins => allFalseAllTrueDisjunctionInference.build(ins) == "true" }) should be(true)
  }

  "classifiers with instance pair label equality constraint " should " have the same value for all instances" in {
    val classifierSameValueTwoInstancesInference = new DummyConstrainedInference(
      Some(allInstancesShouldBeTrue), classifierPositiveScoreForTrue
    )
    instanceSet.forall { ins => classifierSameValueTwoInstancesInference.build(ins) == "true" }
  }

  "trueImpliesTrue " should "work" in {
    val classifierSameValueTwoInstancesInference = new DummyConstrainedInference(
      Some(trueImpliesTrue), classifierNegativeScoreForTrue
    )
    classifierSameValueTwoInstancesInference.build(instanceSet(0)) == "true" &&
      classifierSameValueTwoInstancesInference.build(instanceSet(1)) == "true"
  }

  "trueImpliesFalse " should "work" in {
    val classifierSameValueTwoInstancesInference = new DummyConstrainedInference(
      Some(trueImpliesFalse), classifierNegativeScoreForTrue
    )
    classifierSameValueTwoInstancesInference.build(instanceSet(0)) == "true" &&
      classifierSameValueTwoInstancesInference.build(instanceSet(1)) == "false"
  }

  "falseImpliesTrue " should "work" in {
    val classifierSameValueTwoInstancesInference = new DummyConstrainedInference(
      Some(falseImpliesTrue), classifierNegativeScoreForTrue
    )
    classifierSameValueTwoInstancesInference.build(instanceSet(0)) == "false" &&
      classifierSameValueTwoInstancesInference.build(instanceSet(1)) == "true"
  }

  "falseImpliesFalse " should "work" in {
    val classifierSameValueTwoInstancesInference = new DummyConstrainedInference(
      Some(falseImpliesFalse), classifierNegativeScoreForTrue
    )
    classifierSameValueTwoInstancesInference.build(instanceSet(0)) == "false" &&
      classifierSameValueTwoInstancesInference.build(instanceSet(1)) == "false"
  }
}
