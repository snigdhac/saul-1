/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saul.classifier

import edu.illinois.cs.cogcomp.infer.ilp.{ OJalgoHook, GurobiHook, ILPSolver }
import edu.illinois.cs.cogcomp.lbjava.infer.BalasHook
import edu.illinois.cs.cogcomp.saul.datamodel.edge.Edge
import edu.illinois.cs.cogcomp.saul.datamodel.node.Node
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
import edu.illinois.cs.cogcomp.saul.util.Logging

import scala.collection.{ mutable, Iterable }
import scala.reflect.ClassTag

import scala.collection.JavaConverters._

abstract class ConstrainedProblem[T <: AnyRef, HEAD <: AnyRef](
  implicit
  val tType: ClassTag[T],
  implicit val headType: ClassTag[HEAD]
) extends Logging {
  import ConstrainedProblem._

  protected def estimator: LBJLearnerEquivalent
  protected def constraintsOpt: Option[SaulConstraint[HEAD]] = None

  protected sealed trait SolverType
  protected case object Gurobi extends SolverType
  protected case object OJAlgo extends SolverType
  protected case object Balas extends SolverType
  protected def solverType: SolverType = OJAlgo

  protected sealed trait OptimizationType
  protected case object Max extends OptimizationType
  protected case object Min extends OptimizationType
  protected def optimizationType: OptimizationType = Max

  def apply(t: T): String = ""

  /** The function is used to filter the generated candidates from the head object; remember that the inference starts
    * from the head object. This function finds the objects of type `T` which are connected to the target object of
    * type `HEAD`. If we don't define `filter`, by default it returns all objects connected to `HEAD`.
    * The filter is useful for the `JointTraining` when we go over all global objects and generate all contained object
    * that serve as examples for the basic classifiers involved in the `JoinTraining`. It is possible that we do not
    * want to use all possible candidates but some of them, for example when we have a way to filter the negative
    * candidates, this can come in the filter.
    */
  protected def filter(t: T, head: HEAD): Boolean = true

  /** The `pathToHead` returns only one object of type HEAD, if there are many of them i.e. `Iterable[HEAD]` then it
    * simply returns the `head` of the `Iterable`
    */
  protected def pathToHead: Option[Edge[T, HEAD]] = None

  private def deriveTestInstances: Iterable[T] = pathToHead.map(_.from.getTestingInstances).getOrElse(Iterable.empty)

  private def getCandidates(head: HEAD): Seq[T] = {
    if (tType.equals(headType) || pathToHead.isEmpty) {
      Seq(head.asInstanceOf[T])
    } else {
      val l = pathToHead.get.backward.neighborsOf(head)
      l.size match {
        case 0 =>
          logger.error("Failed to find part")
          Seq.empty[T]
        case _ => l.filter(filter(_, head)).toSeq
      }
    }
  }

  def findHead(x: T): Option[HEAD] = {
    if (tType.equals(headType) || pathToHead.isEmpty) {
      Some(x.asInstanceOf[HEAD])
    } else {
      val l = pathToHead.get.forward.neighborsOf(x).toSet.toSeq
      l.length match {
        case 0 =>
          logger.error("Warning: Failed to find head")
          None
        case 1 =>
          logger.info(s"Found head ${l.head} for child $x")
          Some(l.head)
        case _ =>
          logger.warn("Find too many heads; this is usually because some instances belong to multiple 'head's")
          Some(l.head)
      }
    }
  }

  private def getSolverInstance: ILPSolver = solverType match {
    case OJAlgo => new OJalgoHook()
    case Gurobi => new GurobiHook()
    case Balas => new BalasHook()
    case _ => throw new Exception("Hook not found! ")
  }

  def build(t: T): String = {
    findHead(t) match {
      case Some(head) => build(head, t)
      case None => throw new Exception("Unknown head object")
    }
  }

  private def build(head: HEAD, t: T)(implicit d: DummyImplicit): String = {
    // create a new solver instance
    val solver = getSolverInstance

    // populate the instances connected to head
    val candidates = getCandidates(head)
    println("*** candidates = " + candidates)
    addVariablesToInferenceProblem(candidates, estimator, solver)

    println("estimatorToSolverLabelMap = " + estimatorToSolverLabelMap)

    // populate the constraints and relevant variables
    //println("constraintsOpt = ")
    println(constraintsOpt)
    constraintsOpt.foreach {
      case constraints =>
        //println("constraints = ")
        //        println(constraints)
        val inequalities = processConstraints(constraints, solver)
        //        inequalities.foreach { inequality =>
        //          solver.addLessThanConstraint(inequality.x, inequality.a, inequality.b)
        //        }
        println("final inequalities  . .  . ")
        inequalities.foreach { ineq =>
          println("------")
          println("ineq.x = " + ineq.x.toSeq)
          println("ineq.a = " + ineq.a.toSeq)
          println("ineq.b = " + ineq.b)
          solver.addGreaterThanConstraint(ineq.x, ineq.a, ineq.b)
        }
    }

    solver.solve()

    val estimatorSpecificMap = estimatorToSolverLabelMap.get(estimator).get.asInstanceOf[mutable.Map[T, Seq[(Int, String)]]]

    println(" ----- after solving it ------ ")
    (1 to 30).foreach{ int =>
      println("solver.getIntegerValue(int) = " + solver.getIntegerValue(int))
      println("solver.getBooleanValue(int) = " + solver.getBooleanValue(int))
      println("-------")
    }

    //println("# of candidates: " + candidates.length)
    //println("length of instanceLabelVarMap: " + estimatorToSolverLabelMap.size)
    //println("length of instanceLabelVarMap: " + estimatorToSolverLabelMap.get(estimator).get.size)
    estimatorSpecificMap.get(t) match {
      case Some(indexLabelPairs) =>
        val values = indexLabelPairs.map {
          case (ind, label) =>
            solver.getIntegerValue(ind)
        }
        assert(values.sum == 1, "exactly one label should be active.")

        indexLabelPairs.collectFirst {
          case (ind, label) if solver.getIntegerValue(ind) == 1.0 => label
        }.get
      case None => throw new Exception("instance is not cached ... weird! :-/ ")
    }

    /*val name = head.toString + head.hashCode()
    if(InferenceManager.problemNames.contains(name)){

    } else {
      logger.warn(s"Inference $name has not been cached; running inference . . . ")
    }*/
  }

  //def solve(): Boolean = ??? /// solver.solve()

  /** Test Constrained Classifier with automatically derived test instances.
    *
    * @return Seq of ???
    */
  /*  def test(): Results = {
    test(deriveTestInstances)
  }

  /** Test with given data, use internally
    *
    * @param testData if the collection of data (which is and Iterable of type T) is not given it is derived from the data model based on its type
    * @param exclude it is the label that we want to exclude for evaluation, this is useful for evaluating the multi-class classifiers when we need to measure overall F1 instead of accuracy and we need to exclude the negative class
    * @param outFile The file to write the predictions (can be `null`)
    * @return Seq of ???
    */
  def test(testData: Iterable[T] = null, outFile: String = null, outputGranularity: Int = 0, exclude: String = ""): Results = {
    println()

    val testReader = new IterableToLBJavaParser[T](if (testData == null) deriveTestInstances else testData)
    testReader.reset()

    val tester: TestDiscrete = new TestDiscrete()
    TestWithStorage.test(tester, classifier, onClassifier.getLabeler, testReader, outFile, outputGranularity, exclude)
    val perLabelResults = tester.getLabels.map {
      label =>
        ResultPerLabel(label, tester.getF1(label), tester.getPrecision(label), tester.getRecall(label),
          tester.getAllClasses, tester.getLabeled(label), tester.getPredicted(label), tester.getCorrect(label))
    }
    val overalResultArray = tester.getOverallStats()
    val overalResult = OverallResult(overalResultArray(0), overalResultArray(1), overalResultArray(2))
    Results(perLabelResults, ClassifierUtils.getAverageResults(perLabelResults), overalResult)
  }*/

  //  val a = SaulForAll(
  //    Set(
  //      SaulForAll(
  //        Set(
  //          SaulPairDisjunction(
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #8),Some(true),None),
  //            SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #6),Some(true),None)))
  //            ),
  //          SaulPairDisjunction(
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #9),Some(true),None),
  //            SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #6),Some(true),None)))
  //          ),
  //          SaulPairDisjunction(
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #1),Some(true),None),
  //            SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #2),Some(true),None),
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #3),Some(true),None),
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #4),Some(true),None),
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #5),Some(true),None)))),
  //          SaulPairDisjunction(
  //            SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #6),Some(true),None),
  //            SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #7),Some(true),None),
  //  SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #8),Some(true),None), SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #9),Some(true),None)))), SaulPairDisjunction(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #4),Some(true),None),SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #1),Some(true),None)))), SaulPairDisjunction(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #2),Some(true),None),SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #1),Some(true),None)))), SaulPairDisjunction(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #7),Some(true),None),SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #6),Some(true),None)))), SaulPairDisjunction(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #5),Some(true),None),SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #1),Some(true),None)))), SaulPairDisjunction(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #3),Some(true),None),SaulExists(Set(SaulPropositionalEqualityConstraint(edu.illinois.cs.cogcomp.saulexamples.setcover.SetCoverSolverDataModel2$$anon$1@5d47c63f,Some(neighborhood #1),Some(true),None))) )
  //        )
  //      )
  //    )
  //  )

}

object ConstrainedProblem {
  /*
  def processConstraints2[V](instance: V, saulConstraint: SaulConstraint[V], solver: ILPSolver): Unit = {

    saulConstraint match {
      case c: SaulFirstOrderConstraint[V] => // do nothing
      case c: SaulPropositionalConstraint[V] =>
        addVariablesToInferenceProblem(Seq(instance), c.estimator, solver)
    }

    saulConstraint match {
      case c: SaulPropositionalEqualityConstraint[V] =>
        // estimates per instance
        val estimatorScoresMap = estimatorToSolverLabelMap.get(c.estimator).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]
        val (indices, labels) = estimatorScoresMap.get(instance).get.unzip
        assert(
          c.inequalityValOpt.isEmpty && c.equalityValOpt.isEmpty,
          s"the equality constraint $c is not completely defined"
        )
        assert(
          c.inequalityValOpt.isDefined && c.equalityValOpt.isDefined,
          s"the equality constraint $c has values for both equality and inequality"
        )
        if (c.equalityValOpt.isDefined) {
          // first make sure the target value is valid
          require(
            c.estimator.classifier.allowableValues().toSet.contains(c.equalityValOpt.get),
            s"The target value ${c.equalityValOpt} is not a valid value for classifier ${c.estimator}"
          )
          val labelIndexOpt = labels.zipWithIndex.collectFirst { case (label, idx) if label == c.equalityValOpt.get => idx }
          val labelIndex = labelIndexOpt.getOrElse(
            throw new Exception()
          )
          val coeffs = Array.fill(indices.length) { 0.0 }
          coeffs(labelIndex) = 1.0
          solver.addEqualityConstraint(indices.toArray, coeffs, 1)
        } else {
          require(
            c.estimator.classifier.allowableValues().toSet.contains(c.inequalityValOpt.get),
            s"The target value ${c.inequalityValOpt} is not a valid value for classifier ${c.estimator}"
          )
          val labelIndexOpt = labels.zipWithIndex.collectFirst { case (label, idx) if label == c.inequalityValOpt.get => idx }
          val labelIndex = labelIndexOpt.getOrElse(
            throw new Exception()
          )
          val coeffs = Array.fill(1) { 1.0 }
          solver.addEqualityConstraint(Array(indices(labelIndex)), coeffs, 0)
        }
      case c: SaulConjunction[V] =>
      case c: SaulDisjunction[V] =>
      case c: SaulImplication[V, _] =>
      case c: SaulNegation[V] =>
      case c: SaulFirstOrderDisjunctionConstraint2[V, _] =>
      case c: SaulFirstOrderConjunctionConstraint2[V, _] =>
      case c: SaulFirstOrderAtLeastConstraint2[V, _] =>
      case c: SaulFirstOrderAtMostConstraint2[V, _] =>
      // case   c: SaulConstraint[T] =>
      // case c: SaulFirstOrderConstraint[T] =>
      // case c: SaulPropositionalConstraint[T] =>
    }
  }
*/

  //  sealed trait ILPInequality{
  //    def a: Array[Double]
  //    def x: Array[Int]
  //    def b: Double
  //  }
  // greater or equal to: ax >= b
  case class ILPInequalityGEQ(a: Array[Double], x: Array[Int], b: Double) //extends ILPInequality
  // less or equal to: ax <= b
  //case class ILPInequalityLEQ(a: Array[Double], x: Array[Int], b: Double) extends ILPInequality
  // equal to: ax = b
  //case class ILPInequalityEQ(a: Array[Double], x: Array[Int], b: Double) extends ILPInequality

  def processConstraints[V <: Any](saulConstraint: SaulConstraint[V], solver: ILPSolver)(implicit tag: ClassTag[V]): Set[ILPInequalityGEQ] = {

    //println("SaulConstraint: " + saulConstraint)
/*    saulConstraint match {
      case c: SaulPropositionalConstraint[V] =>
        addVariablesToInferenceProblem(Seq(instance), c.estimator, solver)
      case _ => // do nothing
    }*/

    saulConstraint match {
      case c: SaulPropositionalEqualityConstraint[V] =>
        assert(c.instanceOpt.isDefined, "the instance in the constraint should definitely be defined.")

        // add the missing variables to the map
        addVariablesToInferenceProblem(Seq(c.instanceOpt.get), c.estimator, solver)

        // estimates per instance
        val estimatorScoresMap = estimatorToSolverLabelMap.get(c.estimator).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]

        println("****** c.instanceOpt.get = " + c.instanceOpt.get)
        val (ilpIndices, labels) = estimatorScoresMap.get(c.instanceOpt.get) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.unzip
          case None =>
            val confidenceScores = c.estimator.classifier.scores(c).toArray.map(_.score)
            val labels = c.estimator.classifier.scores(c.instanceOpt.get).toArray.map(_.value)
            val indicesPerLabels = solver.addDiscreteVariable(confidenceScores)
            estimatorScoresMap += (c.instanceOpt.get -> indicesPerLabels.zip(labels) )
            estimatorToSolverLabelMap.put(c.estimator, estimatorScoresMap)
            (indicesPerLabels.toSeq, labels.toSeq)
        }

        assert(
          c.inequalityValOpt.isEmpty || c.equalityValOpt.isEmpty,
          s"the equality constraint $c is not completely defined"
        )
        assert(
          c.inequalityValOpt.isDefined || c.equalityValOpt.isDefined,
          s"the equality constraint $c has values for both equality and inequality"
        )

        if (c.equalityValOpt.isDefined) {
          // first make sure the target value is valid
          require(
            c.estimator.classifier.allowableValues().toSet.contains(c.equalityValOpt.get),
            s"The target value ${c.equalityValOpt} is not a valid value for classifier ${c.estimator}"
          )
          val labelIndexOpt = labels.zipWithIndex.collectFirst { case (label, idx) if label == c.equalityValOpt.get => idx }
          val x = ilpIndices(labelIndexOpt.getOrElse(throw new Exception(s"the corresponding index to label ${c.equalityValOpt.get} not found")))

          // 1.0 x >= 1 : possible only when x = 1
          val a = Array(1.0)
          val b = 1.0
          Set(ILPInequalityGEQ(a, Array(x), b))
        } else {
          require(
            c.estimator.classifier.allowableValues().toSet.contains(c.inequalityValOpt.get),
            s"The target value ${c.inequalityValOpt} is not a valid value for classifier ${c.estimator}"
          )
          val labelIndexOpt = labels.zipWithIndex.collectFirst { case (label, idx) if label == c.inequalityValOpt.get => idx }
          val x = ilpIndices(labelIndexOpt.getOrElse(throw new Exception(s"the corresponding index to label ${c.equalityValOpt.get} not found")))
          val a = Array(1.0)
          val b = 1.0
          // -1 x >= 0 : possible only when x = 0
          Set(ILPInequalityGEQ(a, Array(x), b))
        }
      case c: SaulPairConjunction[V, Any] =>
        val InequalitySystem1 = processConstraints(c.c1, solver)
        val InequalitySystem2 = processConstraints(c.c2, solver)

        // conjunction is simple; you just include all the inequalities
        InequalitySystem1 union InequalitySystem2
      case c: SaulPairDisjunction[V, Any] =>
        val InequalitySystem1 = processConstraints(c.c1, solver)
        val InequalitySystem2 = processConstraints(c.c2, solver)
        val y = solver.addBooleanVariable(0.0)

        // a1.x >= b1 or a2.x >= b2:
        // should be converted to a1.x >= b1.y + min(a1.x).(1-y) or a2.x >= b2.(1-y) + min(a2.x).y
        // We can summarize the first one as:
        // newA = [a1, min(a1.x)-b1]
        // newX = [x, y]
        // newB = min(a1.x)
        val InequalitySystem1New = InequalitySystem1.map { ins =>
          val minValue = (ins.a.filter(_ < 0) :+ 0.0).sum
          val a1New = ins.a :+ (minValue - ins.b)
          val x1New = ins.x :+ y
          val b1New = minValue
          ILPInequalityGEQ(a1New, x1New, b1New)
        }
        val InequalitySystem2New = InequalitySystem2.map { ins =>
          val minValue = (ins.a.filter(_ < 0) :+ 0.0).sum
          val a2New = ins.a :+ (minValue - ins.b)
          val x2New = ins.x :+ y
          val b2New = minValue
          ILPInequalityGEQ(a2New, x2New, b2New)
        }
        InequalitySystem1New union InequalitySystem2New
      case c: SaulNegation[V] =>
        // change the signs of the coefficients
        val InequalitySystemToBeNegated = processConstraints(c.p, solver)
        InequalitySystemToBeNegated.map { in =>
          val minusA = in.a.map(-_)
          val minusB = -in.b
          ILPInequalityGEQ(minusA, in.x, minusB)
        }
      case c: SaulAtLeast[V, Any] =>
        // for each inequality ax >= b we introduce a binary variable y
        // and convert the constraint to ax >= by + (1-y)min(ax)
        // newA = [a, min(ax)-b]
        // newX = [x, y]
        // newB = min(ax)
        println("estimatorToSolverLabelMap = " + estimatorToSolverLabelMap)
        println("c.constraints = " + c.constraints)
        val InequalitySystemsAtLeast = c.constraints.map { processConstraints(_, solver) }
        println("InequalitySystemsAtLeast = ")
        println(InequalitySystemsAtLeast)
        InequalitySystemsAtLeast.foreach{
          _.foreach{ ins =>
            println("a = " + ins.a.toSeq)
            println("x = " + ins.x.toSeq)
            println("b = " + ins.b)
          }
        }
        val (inequalities, newAuxillaryVariables) = InequalitySystemsAtLeast.map { inequalitySystem =>
          val y = solver.addBooleanVariable(-1.0)
          val newInequalities = inequalitySystem.map { inequality =>
            val minValue = (inequality.a.filter(_ < 0) :+ 0.0).sum
            val newA = inequality.a :+ (minValue - inequality.b)
            val newX = inequality.x :+ y
            val newB = minValue
            ILPInequalityGEQ(newA, newX, newB)
          }
          (newInequalities, y)
        }.unzip

        // add a new constraint: at least k constraints should be active
        inequalities.flatten + ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => 1.0), newAuxillaryVariables.toArray, c.k)
      case c: SaulAtMost[V, Any] =>
        val InequalitySystemsAtMost = c.constraints.map { processConstraints(_, solver) }
        // for each inequality ax >= b we introduce a binary variable y
        // and convert the constraint to ax >= by + (1-y)min(ax)
        // newA = [a, min(ax)-b]
        // newX = [x, y]
        // newB = min(ax)
        val InequalitySystemsAtLeast = c.constraints.map { processConstraints(_, solver) }
        val (inequalities, newAuxillaryVariables) = InequalitySystemsAtLeast.map { inequalitySystem =>
          val y = solver.addBooleanVariable(0.0)
          val newInequalities = inequalitySystem.map { inequality =>
            val minValue = inequality.a.filter(_ < 0).sum
            val newA = inequality.a :+ (minValue - inequality.b)
            val newX = inequality.x :+ y
            val newB = minValue
            ILPInequalityGEQ(newA, newX, newB)
          }
          (newInequalities, y)
        }.unzip
        // add a new constraint: at least k constraints should be active
        inequalities.flatten + ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => -1.0), newAuxillaryVariables.toArray, -c.k)
      //case c: SaulExists[V, Any] =>
      //  val InequalitySystemsAtMost = c.constraints.map { processConstraints(instance, _, solver) }
      case c: SaulForAll[V, Any] =>
        c.constraints.flatMap { processConstraints(_, solver) }
    }
  }

  // if the estimator has never been seen before, add its labels to the map
  def createEstimatorSpecificCache[V](estimator: LBJLearnerEquivalent): Unit = {
    if (!estimatorToSolverLabelMap.keySet.contains(estimator)) {
      estimatorToSolverLabelMap += (estimator -> mutable.Map[V, Seq[(Int, String)]]())
    }
  }

  def addVariablesToInferenceProblem[V](instances: Seq[V], estimator: LBJLearnerEquivalent, solver: ILPSolver): Unit = {
    createEstimatorSpecificCache(estimator)

    // estimates per instance
    val estimatorScoresMap = estimatorToSolverLabelMap.get(estimator).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]

    // adding the estimates to the solver and to the map
    instances.foreach { c =>
      println("-- instance = " + c)
      val confidenceScores = estimator.classifier.scores(c).toArray.map(_.score)
      //require(confidenceScores.forall(_ >= 0.0), s"Some of the scores returned by $estimator are below zero.")
      val labels = estimator.classifier.scores(c).toArray.map(_.value)
      println("labels = " + labels.toSeq)
      val instanceIndexPerLabel = solver.addDiscreteVariable(confidenceScores)
      println("instanceIndexPerLabel = " + instanceIndexPerLabel.toSeq)
      if (!estimatorScoresMap.contains(c)) {
        estimatorScoresMap += (c -> instanceIndexPerLabel.zip(labels).toSeq)
      }
    }

    println("right after creating the variables: ")
    println("estimatorScoresMap = " + estimatorScoresMap)

    // add the variables back into the map
    estimatorToSolverLabelMap.put(estimator, estimatorScoresMap)
  }

  import collection._

  // cached results
  //  val cachedResults = mutable.Map[String, mutable.Map[String, Int]]()

  // for each estimator, maps the label of the estimator, to the integer label of the solver
  val estimatorToSolverLabelMap = mutable.Map[LBJLearnerEquivalent, mutable.Map[_, Seq[(Int, String)]]]()

  // for each estimator, maps the integer label of the solver to the label of the estimator
  //  val solverToEstimatorLabelMap = mutable.Map[String, mutable.Map[Int, String]]()
}

object SaulConstraint {
  implicit class LearnerToFirstOrderConstraint(estimator: LBJLearnerEquivalent) {
    def on2[T](newInstance: T)(implicit tag: ClassTag[T]): SaulPropositionalEqualityConstraint[T] = {
      new SaulPropositionalEqualityConstraint[T](estimator, Some(newInstance), None, None)
    }
  }

  implicit def FirstOrderConstraint[T <: AnyRef](coll: Traversable[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)

  implicit def FirstOrderConstraint[T <: AnyRef](coll: Set[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)

  implicit def FirstOrderConstraint[T <: AnyRef](coll: java.util.Collection[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.asScala.toSeq)

  implicit def FirstOrderConstraint[T <: AnyRef](coll: mutable.LinkedHashSet[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)

  implicit def FirstOrderConstraint[T <: AnyRef](node: Node[T]): ConstraintObjWrapper[T] = {
    new ConstraintObjWrapper[T](node.getAllInstances.toSeq)
  }
}

class ConstraintObjWrapper[T](coll: Seq[T]) {
  def ForAll[U](sensors: T => SaulConstraint[U])(implicit tag: ClassTag[T]): SaulForAll[T, U] = {
    new SaulForAll[T, U](coll.map(sensors).toSet)
  }
  def Exists[U](sensors: T => SaulConstraint[U])(implicit tag: ClassTag[T]): SaulAtLeast[T, U] = {
    new SaulAtLeast[T, U](coll.map(sensors).toSet, 1)
  }
  def AtLeast[U](k: Int)(sensors: T => SaulConstraint[U])(implicit tag: ClassTag[T]): SaulAtLeast[T, U] = {
    new SaulAtLeast[T, U](coll.map(sensors).toSet, k)
  }
  def AtMost[U](k: Int)(sensors: T => SaulConstraint[U])(implicit tag: ClassTag[T]): SaulAtMost[T, U] = {
    new SaulAtMost[T, U](coll.map(sensors).toSet, k)
  }
}

sealed trait SaulConstraint[T] {
  def and4[U](cons: SaulConstraint[U]) = {
    new SaulPairConjunction[T, U](this, cons)
  }

  def or4[U](cons: SaulConstraint[U]) = {
    new SaulPairDisjunction[T, U](this, cons)
  }

  //  def implies[U](q: SaulConstraint[U]): SaulImplication[T, U] = {
  //    new SaulImplication[T, U](this, q)
  //  }
  //
  //  def ====>[U](q: SaulConstraint[U]): SaulImplication[T, U] = implies(q)

  def implies[U](q: SaulConstraint[U]): SaulPairConjunction[T, U] = {
    //new SaulImplication[T, U](this, q)
    // p --> q can be modelled as p or not(q)
    SaulPairConjunction[T, U](this, SaulNegation(q))
  }

  def ====>[U](q: SaulConstraint[U]): SaulPairConjunction[T, U] = implies(q)

  def negate: SaulNegation[T] = {
    new SaulNegation(this)
  }

  def unary_! = negate
}

// zero-th order constraints
sealed trait SaulPropositionalConstraint[T] extends SaulConstraint[T] {
  def estimator: LBJLearnerEquivalent
}

case class SaulPropositionalEqualityConstraint[T](
  estimator: LBJLearnerEquivalent,
  instanceOpt: Option[T],
  equalityValOpt: Option[String],
  inequalityValOpt: Option[String]
) extends SaulPropositionalConstraint[T] {
  def is2(targetValue: String): SaulPropositionalEqualityConstraint[T] = new SaulPropositionalEqualityConstraint[T](estimator, instanceOpt, Some(targetValue), None)
  def isTrue2 = is2("true")
  def isFalse2 = is2("false")
  def isNot2(targetValue: String): SaulPropositionalEqualityConstraint[T] = new SaulPropositionalEqualityConstraint[T](estimator, instanceOpt, None, Some(targetValue))
}

case class SaulPairConjunction[T, U](c1: SaulConstraint[T], c2: SaulConstraint[U]) extends SaulConstraint[T]

case class SaulPairDisjunction[T, U](c1: SaulConstraint[T], c2: SaulConstraint[U]) extends SaulConstraint[T]

case class SaulForAll[T, U](constraints: Set[SaulConstraint[U]]) extends SaulConstraint[T]

case class SaulAtLeast[T, U](constraints: Set[SaulConstraint[U]], k: Int) extends SaulConstraint[T]

case class SaulAtMost[T, U](constraints: Set[SaulConstraint[U]], k: Int) extends SaulConstraint[T]

case class SaulImplication[T, U](p: SaulConstraint[T], q: SaulConstraint[U]) extends SaulConstraint[T]

case class SaulNegation[T](p: SaulConstraint[T]) extends SaulConstraint[T]
