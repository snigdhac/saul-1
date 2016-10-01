/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saul.infer

import java.io.PrintStream

import edu.illinois.cs.cogcomp.lbjava.classify.{FeatureVector, ScoreSet}
import edu.illinois.cs.cogcomp.lbjava.learn.Learner
import edu.illinois.cs.cogcomp.saul.classifier.{ConstrainedProblem, SaulConstraint}
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
import org.scalatest.{Matchers, FlatSpec}

class StaticClassifier extends Learner("DummyClassifer") {
  override def getInputType: String = { "DummyInstance" }

  override def allowableValues: Array[String] = { Array[String]("false", "true") }

  override def equals(o: Any): Boolean = { getClass == o.getClass }

  /** The reason for true to be -1 is because the internal optimization by default finds the maximizer, while in this
    * problem we are looking for a minimizer
    */
  override def scores(example: AnyRef): ScoreSet = {
    val result: ScoreSet = new ScoreSet
    result.put("false", 0)
    result.put("true", 1)
    result
  }

  override def write(printStream: PrintStream): Unit = ???

  override def scores(ints: Array[Int], doubles: Array[Double]): ScoreSet = ???

  override def classify(ints: Array[Int], doubles: Array[Double]): FeatureVector = ???

  override def learn(ints: Array[Int], doubles: Array[Double], ints1: Array[Int], doubles1: Array[Double]): Unit = ???
}

object DummyDataModel extends DataModel {

  case class Instance(value: Int)

  val instances = node[Instance]

  /** definition of the constraints */
  val containStation: LBJLearnerEquivalent = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier()
  }

  import SaulConstraint._

  def forAllTrue = instances.ForAll { x: Instance => containStation on2 x isTrue2 }
  def forAllFalse = instances.ForAll { x: Instance => containStation on2 x isFalse2 }
  def existsTrue = instances.Exists { x: Instance => containStation on2 x isTrue2 }
  def existsFalse = instances.Exists { x: Instance => containStation on2 x isFalse2 }
  def atLeastTrue(k: Int) = instances.AtLeast(k) { x: Instance => containStation on2 x isTrue2 }
  def atLeastFalse(k: Int) = instances.AtLeast(k) { x: Instance => containStation on2 x isFalse2 }
  def atMostTrue(k: Int) = instances.AtMost(k) { x: Instance => containStation on2 x isTrue2 }
  def atMostFalse(k: Int) = instances.AtMost(k) { x: Instance => containStation on2 x isFalse2 }
}

class DummyConstrainedInference(someConstraint: Some[SaulConstraint[DummyDataModel.Instance]]) extends ConstrainedProblem[DummyDataModel.Instance, DummyDataModel.Instance] {
  override lazy val estimator = new LBJLearnerEquivalent {
    override val classifier: Learner = new StaticClassifier()
  }
  override def pathToHead = None
  override def constraintsOpt = someConstraint
  override def solverType = OJAlgo
}

class inferenceTest extends FlatSpec with Matchers {
  val instances = (0 to 10).map(DummyDataModel.Instance)
  DummyDataModel.instances.populate(instances)

  // all true
  "ForALl " should " return all true instances" in {
    val allTrueInference = new DummyConstrainedInference(Some(DummyDataModel.forAllTrue))
    instances.foreach { ins => allTrueInference.build(ins) should be("true") }
  }

  // all false
  "ForALl " should " return all false instances" in {
    val allFalseInference = new DummyConstrainedInference(Some(DummyDataModel.forAllFalse))
    instances.foreach { ins => allFalseInference.build(ins) should be("false") }
  }

  // exist true
  "Exists " should " return at least one true instance" in {
    val existOneTrue = new DummyConstrainedInference(Some(DummyDataModel.existsTrue))
    instances.exists { ins => existOneTrue.build(ins) == "true" } should be(true)
  }

  // exist false
  "Exists " should " return at least one false instance" in {
    val existOneFalse = new DummyConstrainedInference(Some(DummyDataModel.existsFalse))
    instances.exists { ins => existOneFalse.build(ins) == "false" } should be(true)
  }

  // at least 2 true
  "AtLeast " should " return at least two true instance" in {
    val atLeastTwoTrue = new DummyConstrainedInference(Some(DummyDataModel.atLeastTrue(2)))
    instances.count { ins => atLeastTwoTrue.build(ins) == "true" } should be >= 2
  }

  // at least 2 false
  "AtLeast " should " return at least two false instance" in {
    val atLeastTwoFalse = new DummyConstrainedInference(Some(DummyDataModel.atLeastFalse(2)))
    instances.count { ins => atLeastTwoFalse.build(ins) == "false" } should be >= 2
  }

  // at least 3 true
  "AtLeast " should " return at least three true instance" in {
    val atLeastThreeTrue = new DummyConstrainedInference(Some(DummyDataModel.atLeastTrue(3)))
    instances.count { ins => atLeastThreeTrue.build(ins) == "true" } should be >= 3
  }

  // at least 3 false
  "AtLeast " should " return at least three false instance" in {
    val atLeastThreeFalse = new DummyConstrainedInference(Some(DummyDataModel.atLeastFalse(3)))
    instances.count{ ins => atLeastThreeFalse.build(ins) == "false" } should be >= 3
  }

  // at most 2 true
  "AtMost " should " return at most two true instance" in {
    val atMostTwoTrue = new DummyConstrainedInference(Some(DummyDataModel.atMostTrue(2)))
    instances.count { ins => atMostTwoTrue.build(ins) == "true" } should be <= 2
  }

  // at most 2 false
  "AtMost " should " return at most two false instance" in {
    val atMostTwoFalse = new DummyConstrainedInference(Some(DummyDataModel.atMostFalse(2)))
    instances.count { ins => atMostTwoFalse.build(ins) == "false" } should be <= 2
  }

  // at most 3 true
  "AtMost " should " return at most three true instance" in {
    val atMostThreeTrue = new DummyConstrainedInference(Some(DummyDataModel.atMostTrue(3)))
    instances.count { ins => atMostThreeTrue.build(ins) == "true" } should be <= 3
  }

  // at most 3 false
  "AtMost " should " return at most three false instance" in {
    val atMostThreeFalse = new DummyConstrainedInference(Some(DummyDataModel.atMostFalse(3)))
    instances.count { ins => atMostThreeFalse.build(ins) == "false" } should be <= 3
  }
}
