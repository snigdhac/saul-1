/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saul.classifier

import edu.illinois.cs.cogcomp.infer.ilp.{ OJalgoHook, GurobiHook, ILPSolver }
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.lbjava.infer.BalasHook
import edu.illinois.cs.cogcomp.saul.datamodel.edge.Edge
import edu.illinois.cs.cogcomp.saul.datamodel.node.Node
import edu.illinois.cs.cogcomp.saul.lbjrelated.LBJLearnerEquivalent
import edu.illinois.cs.cogcomp.saul.parser.IterableToLBJavaParser
import edu.illinois.cs.cogcomp.saul.test.TestWithStorage
import edu.illinois.cs.cogcomp.saul.util.Logging

import scala.collection.{ mutable, Iterable }
import scala.reflect.ClassTag

import scala.collection.JavaConverters._

abstract class ConstrainedClassifier[T <: AnyRef, HEAD <: AnyRef](
  implicit
  val tType: ClassTag[T],
  implicit val headType: ClassTag[HEAD]
) extends Logging {

  def onClassifier: LBJLearnerEquivalent
  protected def constraintsOpt: Option[Constraint[HEAD]] = None

  protected sealed trait SolverType
  protected case object Gurobi extends SolverType
  protected case object OJAlgo extends SolverType
  protected case object Balas extends SolverType
  protected def solverType: SolverType = OJAlgo

  protected sealed trait OptimizationType
  protected case object Max extends OptimizationType
  protected case object Min extends OptimizationType
  protected def optimizationType: OptimizationType = Max

  private val inferenceManager = new InferenceManager()

  def getClassSimpleNameForClassifier = this.getClass.getSimpleName

  def apply(t: T): String = build(t)

  /** The function is used to filter the generated candidates from the head object; remember that the inference starts
    * from the head object. This function finds the objects of type [[T]] which are connected to the target object of
    * type [[HEAD]]. If we don't define [[filter]], by default it returns all objects connected to [[HEAD]].
    * The filter is useful for the `JointTraining` when we go over all global objects and generate all contained object
    * that serve as examples for the basic classifiers involved in the `JoinTraining`. It is possible that we do not
    * want to use all possible candidates but some of them, for example when we have a way to filter the negative
    * candidates, this can come in the filter.
    */
  protected def filter(t: T, head: HEAD): Boolean = true

  /** The [[pathToHead]] returns only one object of type HEAD, if there are many of them i.e. `Iterable[HEAD]` then it
    * simply returns the head of the [[Iterable]]
    */
  protected def pathToHead: Option[Edge[T, HEAD]] = None

  private def deriveTestInstances: Iterable[T] = {
    pathToHead.map(edge => edge.from)
      .orElse({
        onClassifier match {
          case clf: Learnable[T] => Some(clf.node)
          case _ => logger.error("pathToHead is not provided and the onClassifier is not a Learnable!"); None
        }
      })
      .map(node => node.getTestingInstances)
      .getOrElse(Iterable.empty)
  }

  def getCandidates(head: HEAD): Seq[T] = {
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
          logger.warn("Found too many heads; this is usually because some instances belong to multiple 'head's")
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

  def cacheKey[U](u: U): String = u.toString

  def getInstancesInvolvedInProblem: Option[Set[_]] = {
    constraintsOpt.map { constraint => getInstancesInvolved(constraint) }
  }

  def getInstancesInvolved(constraint: Constraint[_]): Set[_] = {
    constraint match {
      case c: PropositionalEqualityConstraint[_] =>
        Set(c.instanceOpt.get)
      case c: PairConjunction[_, _] =>
        getInstancesInvolved(c.c1) ++ getInstancesInvolved(c.c2)
      case c: PairDisjunction[_, _] =>
        getInstancesInvolved(c.c1) ++ getInstancesInvolved(c.c2)
      case c: Negation[_] =>
        getInstancesInvolved(c.p)
      case c: AtLeast[_, _] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: AtMost[_, _] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: ForAll[_, _] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: Exactly[_, _] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: EstimatorPairEqualityConstraint[_] =>
        Set(c.instance)
      case c: InstancePairEqualityConstraint[_] =>
        Set(c.instance1, c.instance2Opt.get)
      case c: Implication[_, _] =>
        throw new Exception("this constraint should have been rewritten in terms of other constraints. ")
    }
  }

  private def build(head: HEAD, t: T)(implicit d: DummyImplicit): String = {
    val instancesInvolved = getInstancesInvolvedInProblem
    if (constraintsOpt.isDefined && instancesInvolved.get.isEmpty) {
      logger.warn("there are no instances associated with the constraints. It might be because you have defined " +
        "the constraints with 'val' modifier, instead of 'def'.")
    }
    val instanceIsInvolvedInConstraint = instancesInvolved.exists { set =>
      set.exists {
        case x: T => if (x == t) true else false
        case everythingElse => false
      }
    }
    if (instanceIsInvolvedInConstraint) {
      val mainCacheKey = instancesInvolved.map(cacheKey(_)).toSeq.sorted.mkString("*") + onClassifier.toString + constraintsOpt
      val resultOpt = inferenceManager.cachedResults.get(mainCacheKey)
      resultOpt match {
        case Some(estimatorPredictions) =>
          val labelsPerInstances = estimatorPredictions(onClassifier)
          require(labelsPerInstances.contains(cacheKey(t)), s"Does not contain the cache key for ${cacheKey(t)}")
          labelsPerInstances.get(cacheKey(t)).get
        case None =>
          // create a new solver instance
          val solver = getSolverInstance
          solver.setMaximize(optimizationType == Max)

          // populate the instances connected to head
          val candidates = getCandidates(head)
          inferenceManager.addVariablesToInferenceProblem(candidates, onClassifier, solver)

          constraintsOpt.foreach {
            case constraints =>
              val inequalities = inferenceManager.processConstraints(constraints, solver)
              inequalities.foreach { ineq =>
                solver.addGreaterThanConstraint(ineq.x, ineq.a, ineq.b)
              }
          }

          solver.solve()
          if (!solver.isSolved) {
            logger.warn("Instance not solved . . . ")
          }

          val estimatorSpecificMap = inferenceManager.estimatorToSolverLabelMap.get(onClassifier).get.asInstanceOf[mutable.Map[T, Seq[(Int, String)]]]

          val labelsPerEstimatorPerInstance = inferenceManager.estimatorToSolverLabelMap.mapValues { instanceLabelMap =>
            instanceLabelMap.map {
              case (c, indexLabelPairs) =>
                val instanceKey = cacheKey(c)
                val predictedLabel = indexLabelPairs.collectFirst {
                  case (ind, label) if solver.getIntegerValue(ind) == 1.0 => label
                }.get
                instanceKey -> predictedLabel
            }
          }

          inferenceManager.cachedResults.put(mainCacheKey, labelsPerEstimatorPerInstance)

          estimatorSpecificMap.get(t) match {
            case Some(indexLabelPairs) =>
              val values = indexLabelPairs.map {
                case (ind, _) => solver.getIntegerValue(ind)
              }
              assert(values.sum == 1, "exactly one label should be active.")
              indexLabelPairs.collectFirst {
                case (ind, label) if solver.getIntegerValue(ind) == 1.0 => label
              }.get
            case None => throw new Exception("instance is not cached ... weird! :-/ ")
          }
      }
    } else {
      // if the instance doesn't involve in any constraints, it means that it's a simple non-constrained problem.
      logger.info("getting the label with the highest score . . . ")
      onClassifier.classifier.scores(t).highScoreValue()
    }
  }

  /** Test Constrained Classifier with automatically derived test instances.
    *
    * @return A [[Results]] object
    */
  def test(): Results = {
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
    val testReader = new IterableToLBJavaParser[T](if (testData == null) deriveTestInstances else testData)
    testReader.reset()
    val tester: TestDiscrete = new TestDiscrete()
    TestWithStorage.test(tester, onClassifier.classifier, onClassifier.getLabeler, testReader, outFile, outputGranularity, exclude)
    val perLabelResults = tester.getLabels.map {
      label =>
        ResultPerLabel(label, tester.getF1(label), tester.getPrecision(label), tester.getRecall(label),
          tester.getAllClasses, tester.getLabeled(label), tester.getPredicted(label), tester.getCorrect(label))
    }
    val overalResultArray = tester.getOverallStats()
    val overalResult = OverallResult(overalResultArray(0), overalResultArray(1), overalResultArray(2))
    Results(perLabelResults, ClassifierUtils.getAverageResults(perLabelResults), overalResult)
  }
}

class InferenceManager {
  import collection._

  /** Contains cache of problems already solved. The key is the head object, which maps to instances and their
    * predicted values in the output of inference
    */
  val cachedResults = mutable.Map[String, Map[LBJLearnerEquivalent, Map[String, String]]]()

  // for each estimator, maps the label of the estimator, to the integer label of the solver
  val estimatorToSolverLabelMap = mutable.Map[LBJLearnerEquivalent, mutable.Map[_, Seq[(Int, String)]]]()

  // a small number used in creation of exclusive inequalities
  private val epsilon = 0.01

  // greater or equal to: ax >= b
  case class ILPInequalityGEQ(a: Array[Double], x: Array[Int], b: Double)

  def processConstraints[V <: Any](saulConstraint: Constraint[V], solver: ILPSolver)(implicit tag: ClassTag[V]): Set[ILPInequalityGEQ] = {

    saulConstraint match {
      case c: PropositionalEqualityConstraint[V] =>
        assert(c.instanceOpt.isDefined, "the instance in the constraint should definitely be defined.")

        // add the missing variables to the map
        addVariablesToInferenceProblem(Seq(c.instanceOpt.get), c.estimator, solver)

        // estimates per instance
        val estimatorScoresMap = estimatorToSolverLabelMap.get(c.estimator).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]

        val (ilpIndices, labels) = estimatorScoresMap.get(c.instanceOpt.get) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.unzip
          case None =>
            val confidenceScores = c.estimator.classifier.scores(c).toArray.map(_.score)
            val labels = c.estimator.classifier.scores(c.instanceOpt.get).toArray.map(_.value)
            val indicesPerLabels = solver.addDiscreteVariable(confidenceScores)
            estimatorScoresMap += (c.instanceOpt.get -> indicesPerLabels.zip(labels))
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

          // 1x == 1; which can be written as
          // (a) +1x >= +1
          // (b) -1x >= -1
          // ILPInequalityGEQ(Array(-1.0), Array(x), -1.0)
          Set(ILPInequalityGEQ(Array(1.0), Array(x), 1.0)) //, ILPInequalityGEQ(Array(-1.0), Array(x), -1.0))
        } else {
          require(
            c.estimator.classifier.allowableValues().toSet.contains(c.inequalityValOpt.get),
            s"The target value ${c.inequalityValOpt} is not a valid value for classifier ${c.estimator}"
          )
          val labelIndexOpt = labels.zipWithIndex.collectFirst { case (label, idx) if label == c.inequalityValOpt.get => idx }
          val x = ilpIndices(labelIndexOpt.getOrElse(throw new Exception(s"the corresponding index to label ${c.equalityValOpt.get} not found")))
          // 1 x == 0 : possible only when x = 0, which can be written as
          // (a) +1 x >= 0
          // (b) -1 x >= 0
          Set(ILPInequalityGEQ(Array(-1.0), Array(x), 0.0)) //, ILPInequalityGEQ(Array(1.0), Array(x), 0.0))
        }
      case c: EstimatorPairEqualityConstraint[V] =>
        assert(c.estimator2Opt.isDefined, "the second estimator is not defined for estimator-pair constraint. Weird . . . ")

        // add the missing variables to the map
        addVariablesToInferenceProblem(Seq(c.instance), c.estimator1, solver)
        addVariablesToInferenceProblem(Seq(c.instance), c.estimator2Opt.get, solver)

        // estimates per instance
        val estimatorScoresMap1 = estimatorToSolverLabelMap.get(c.estimator1).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]
        val estimatorScoresMap2 = estimatorToSolverLabelMap.get(c.estimator2Opt.get).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]

        val labelToIndices1 = estimatorScoresMap1.get(c.instance) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.map(_.swap).toMap
          case None => throw new Exception("The instance hasn't been seen??")
        }

        val labelToIndices2 = estimatorScoresMap2.get(c.instance) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.map(_.swap).toMap
          case None => throw new Exception("The instance hasn't been seen??")
        }

        // this requirement might be an overkill, but keeping it for now.
        require(
          labelToIndices1.keySet == labelToIndices2.keySet,
          "the label set for the two classifiers is not the same"
        )

        val yAll = solver.addDiscreteVariable(Array.fill(labelToIndices1.keySet.size) { 0 })

        val labels = labelToIndices1.keySet.toSeq
        labels.indices.flatMap { idx =>
          val label = labels(idx)
          val y = yAll(idx)
          val variable1 = labelToIndices1(label)
          val variable2 = labelToIndices2(label)

          if (c.equalsOpt.get) {
            // for each variable, if y is active, that variable should also be active:
            // 1 variable >= 1 y
            Set(
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable1, y), 0.0),
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable2, y), 0.0)
            )
          } else {
            // for each variable, if y is active, that variable should also be active:
            // variable >= y  which is, variable - y >= 0
            // 1 - variable >= 1 y which is, -variable -y >= -1
            Set(
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable1, y), 0.0),
              ILPInequalityGEQ(Array(-1.0, -1.0), Array(variable1, y), -1.0),
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable2, y), 0.0),
              ILPInequalityGEQ(Array(-1.0, -1.0), Array(variable2, y), -1.0)
            )
          }
        }.toSet

      case c: InstancePairEqualityConstraint[V] =>
        assert(c.instance2Opt.isDefined, "the second instance is not defined for estimator-pair constraint. Weird . . . ")

        // add the missing variables to the map
        addVariablesToInferenceProblem(Seq(c.instance1), c.estimator, solver)
        addVariablesToInferenceProblem(Seq(c.instance2Opt.get), c.estimator, solver)

        // estimates per instance
        val estimatorScoresMap = estimatorToSolverLabelMap.get(c.estimator).get.asInstanceOf[mutable.Map[V, Seq[(Int, String)]]]

        val labelToIndices1 = estimatorScoresMap.get(c.instance1) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.map(_.swap).toMap
          case None => throw new Exception("The instance hasn't been seen??")
        }

        val labelToIndices2 = estimatorScoresMap.get(c.instance2Opt.get) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.map(_.swap).toMap
          case None => throw new Exception("The instance hasn't been seen??")
        }

        // this requirement might be an overkill, but keeping it for now.
        require(
          labelToIndices1.keySet == labelToIndices2.keySet,
          "the label set for the two classifiers is not the same; " +
            "although they belong to the same classifier; weird . . . "
        )

        val yAll = solver.addDiscreteVariable(Array.fill(labelToIndices1.keySet.size) { 0 })

        val labels = labelToIndices1.keySet.toSeq
        labels.indices.flatMap { idx =>
          val label = labels(idx)
          val y = yAll(idx)
          val variable1 = labelToIndices1(label)
          val variable2 = labelToIndices2(label)

          if (c.equalsOpt.get) {
            // for each variable, if y is active, that variable should also be active:
            // 1 variable >= 1 y
            Set(
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable1, y), 0.0),
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable2, y), 0.0)
            )
          } else {
            // for each variable, if y is active, that variable should also be active:
            // variable >= y  which is, variable - y >= 0
            // 1 - variable >= 1 y which is, -variable -y >= -1
            Set(
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable1, y), 0.0),
              ILPInequalityGEQ(Array(-1.0, -1.0), Array(variable1, y), -1.0),
              ILPInequalityGEQ(Array(1.0, -1.0), Array(variable2, y), 0.0),
              ILPInequalityGEQ(Array(-1.0, -1.0), Array(variable2, y), -1.0)
            )
          }
        }.toSet

      case c: PairConjunction[V, Any] =>
        val InequalitySystem1 = processConstraints(c.c1, solver)
        val InequalitySystem2 = processConstraints(c.c2, solver)

        // conjunction is simple; you just include all the inequalities
        InequalitySystem1 union InequalitySystem2
      case c: PairDisjunction[V, Any] => // TODO: how to get rid of these 'Any' types, and maybe replace with _?
        val InequalitySystem1 = processConstraints(c.c1, solver)
        val InequalitySystem2 = processConstraints(c.c2, solver)
        val y1 = solver.addBooleanVariable(0.0)
        val y2 = solver.addBooleanVariable(0.0)
        // a1.x >= b1 or a2.x >= b2:
        // should be converted to
        // a1.x >= b1.y1 + min(a1.x).(1-y1)
        // a2.x >= b2.(1-y2) + min(a2.x).y2
        // y1 + y2 >= 1
        // We can summarize the first one as:
        // newA1 = [a1, min(a1.x)-b1]
        // newX1 = [x, y]
        // newB1 = min(a1.x)
        val InequalitySystem1New = InequalitySystem1.map { ins =>
          val minValue = (ins.a.filter(_ < 0) :+ 0.0).sum
          val a1New = ins.a :+ (minValue - ins.b)
          val x1New = ins.x :+ y1
          val b1New = minValue
          ILPInequalityGEQ(a1New, x1New, b1New)
        }
        val InequalitySystem2New = InequalitySystem2.map { ins =>
          val minValue = (ins.a.filter(_ < 0) :+ 0.0).sum
          val a2New = ins.a :+ (minValue - ins.b)
          val x2New = ins.x :+ y2
          val b2New = minValue
          ILPInequalityGEQ(a2New, x2New, b2New)
        }
        val atLeastOne = ILPInequalityGEQ(Array(1, 1), Array(y1, y2), 1.0)
        InequalitySystem1New union InequalitySystem2New + atLeastOne
      case c: Negation[V] =>
        // change the signs of the coefficients
        val InequalitySystemToBeNegated = processConstraints(c.p, solver)
        InequalitySystemToBeNegated.map { in =>
          val minusA = in.a.map(-_)
          val minusB = -in.b + epsilon
          ILPInequalityGEQ(minusA, in.x, minusB)
        }
      case c: AtLeast[V, Any] =>
        val InequalitySystems = c.constraints.map { processConstraints(_, solver) }
        // for each inequality ax >= b we introduce a binary variable y
        // and convert the constraint to ax >= by + (1-y).min(ax) and ax < (b-e)(1-y) + y.max(ax)
        // which can be written as ax+y(min(ax)-b) >= min(ax) and ax + y(b - e - max(ax)) < b - e
        // newA1 = [a, min(ax) - b]
        // newX1 = [x, y]
        // newB1 = min(ax)
        // newA2 = [-a,  max(ax) - b]
        // newX2 = [x, y]
        // newB2 = -b + epsilon
        val (inequalities, newAuxillaryVariables) = InequalitySystems.map { inequalitySystem =>
          val y = solver.addBooleanVariable(0.0)
          val newInequalities = inequalitySystem.flatMap { inequality =>
            val maxValue = (inequality.a.filter(_ > 0) :+ 0.0).sum
            val minValue = (inequality.a.filter(_ < 0) :+ 0.0).sum
            val newA1 = inequality.a :+ (minValue - inequality.b)
            val newX1 = inequality.x :+ y
            val newB1 = minValue
            val newA2 = inequality.a.map(-_) :+ (maxValue + epsilon - inequality.b)
            val newX2 = inequality.x :+ y
            val newB2 = -inequality.b + epsilon
            Set(ILPInequalityGEQ(newA1, newX1, newB1), ILPInequalityGEQ(newA2, newX2, newB2))
          }
          (newInequalities, y)
        }.unzip
        // add a new constraint: at least k constraints should be active
        inequalities.flatten +
          ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => 1.0), newAuxillaryVariables.toArray, c.k)
      case c: AtMost[V, Any] =>
        val InequalitySystems = c.constraints.map { processConstraints(_, solver) }
        // for each inequality ax >= b we introduce a binary variable y
        // and convert the constraint to ax >= by + (1-y).min(ax) and ax < (b-e)(1-y) + y.max(ax)
        // which can be written as ax+y(min(ax)-b) >= min(ax) and ax + y(b - e - max(ax)) < b - e
        // newA1 = [a, min(ax) - b]
        // newX1 = [x, y]
        // newB1 = min(ax)
        // newA2 = [-a,  max(ax) - b]
        // newX2 = [x, y]
        // newB2 = -b + epsilon
        val (inequalities, newAuxillaryVariables) = InequalitySystems.map { inequalitySystem =>
          val y = solver.addBooleanVariable(0.0)
          val newInequalities = inequalitySystem.flatMap { inequality =>
            val maxValue = (inequality.a.filter(_ > 0) :+ 0.0).sum
            val minValue = (inequality.a.filter(_ < 0) :+ 0.0).sum
            val newA1 = inequality.a :+ (minValue - inequality.b)
            val newX1 = inequality.x :+ y
            val newB1 = minValue
            val newA2 = inequality.a.map(-_) :+ (maxValue + epsilon - inequality.b)
            val newX2 = inequality.x :+ y
            val newB2 = -inequality.b + epsilon
            Set(ILPInequalityGEQ(newA1, newX1, newB1), ILPInequalityGEQ(newA2, newX2, newB2))
          }
          (newInequalities, y)
        }.unzip
        // add a new constraint: at least k constraints should be active
        inequalities.flatten +
          ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => -1.0), newAuxillaryVariables.toArray, -c.k)
      case c: Exactly[V, Any] =>
        val InequalitySystems = c.constraints.map { processConstraints(_, solver) }
        // for each inequality ax >= b we introduce a binary variable y
        // and convert the constraint to ax >= by + (1-y).min(ax) and ax < (b-e)(1-y) + y.max(ax)
        // which can be written as ax+y(min(ax)-b) >= min(ax) and ax + y(b - e - max(ax)) < b - e
        // newA1 = [a, min(ax) - b]
        // newX1 = [x, y]
        // newB1 = min(ax)
        // newA2 = [-a,  max(ax) - b]
        // newX2 = [x, y]
        // newB2 = -b + epsilon
        val (inequalities, newAuxillaryVariables) = InequalitySystems.map { inequalitySystem =>
          val y = solver.addBooleanVariable(0.0)
          val newInequalities = inequalitySystem.flatMap { inequality =>
            val maxValue = (inequality.a.filter(_ > 0) :+ 0.0).sum
            val minValue = (inequality.a.filter(_ < 0) :+ 0.0).sum
            val newA1 = inequality.a :+ (minValue - inequality.b)
            val newX1 = inequality.x :+ y
            val newB1 = minValue
            val newA2 = inequality.a.map(-_) :+ (maxValue + epsilon - inequality.b)
            val newX2 = inequality.x :+ y
            val newB2 = -inequality.b + epsilon
            Set(ILPInequalityGEQ(newA1, newX1, newB1), ILPInequalityGEQ(newA2, newX2, newB2))
          }
          (newInequalities, y)
        }.unzip
        // add a new constraint: at least k constraints should be active
        inequalities.flatten union Set(
          ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => 1.0), newAuxillaryVariables.toArray, c.k),
          ILPInequalityGEQ(newAuxillaryVariables.toArray.map(_ => -1.0), newAuxillaryVariables.toArray, -c.k)
        )

      case c: ForAll[V, Any] =>
        c.constraints.flatMap { processConstraints(_, solver) }

      case c: Implication[_, _] =>
        throw new Exception("Saul implicaton is converted to other operations. ")
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
      val confidenceScores = estimator.classifier.scores(c).toArray.map(_.score)
      val labels = estimator.classifier.scores(c).toArray.map(_.value)
      val instanceIndexPerLabel = solver.addDiscreteVariable(confidenceScores)
      if (!estimatorScoresMap.contains(c)) {
        estimatorScoresMap += (c -> instanceIndexPerLabel.zip(labels).toSeq)
      }
    }

    // add the variables back into the map
    estimatorToSolverLabelMap.put(estimator, estimatorScoresMap)
  }
}

object Constraint {
  implicit class LearnerToFirstOrderConstraint1(estimator: LBJLearnerEquivalent) {
    // connecting a classifier to a specific instance
    def on[T](newInstance: T)(implicit tag: ClassTag[T]): InstanceWrapper[T] = new InstanceWrapper(newInstance, estimator)
  }

  implicit def toPropositionalEqualityConstraint[T](wrapper: InstanceWrapper[T])(implicit tag: ClassTag[T]): PropositionalEqualityConstraint[T] = {
    new PropositionalEqualityConstraint[T](wrapper.estimator, Some(wrapper.instance), None, None)
  }
  implicit def toInstancePairEqualityConstraint[T](wrapper: InstanceWrapper[T])(implicit tag: ClassTag[T], d1: DummyImplicit): InstancePairEqualityConstraint[T] = {
    new InstancePairEqualityConstraint[T](wrapper.estimator, wrapper.instance, None, None)
  }
  implicit def toEstimatorPairEqualityConstraint[T](wrapper: InstanceWrapper[T])(implicit tag: ClassTag[T]): EstimatorPairEqualityConstraint[T] = {
    new EstimatorPairEqualityConstraint[T](wrapper.estimator, None, wrapper.instance, None)
  }

  // collection of target object types
  implicit def firstOrderConstraint[T <: AnyRef](coll: Traversable[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)
  implicit def firstOrderConstraint[T <: AnyRef](coll: Set[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)
  implicit def firstOrderConstraint[T <: AnyRef](coll: java.util.Collection[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.asScala.toSeq)
  implicit def firstOrderConstraint[T <: AnyRef](coll: mutable.LinkedHashSet[T]): ConstraintObjWrapper[T] = new ConstraintObjWrapper[T](coll.toSeq)
  implicit def firstOrderConstraint[T <: AnyRef](node: Node[T]): ConstraintObjWrapper[T] = {
    new ConstraintObjWrapper[T](node.getAllInstances.toSeq)
  }

  // collection of constraints
  implicit def createConstraintCollection[T <: AnyRef](coll: Traversable[Constraint[T]]): ConstraintCollection[T, T] = new ConstraintCollection[T, T](coll.toSet)
  implicit def createConstraintCollection[T <: AnyRef](coll: Set[Constraint[T]]): ConstraintCollection[T, T] = new ConstraintCollection[T, T](coll)
  implicit def createConstraintCollection[T <: AnyRef](coll: java.util.Collection[Constraint[T]]): ConstraintCollection[T, T] = new ConstraintCollection[T, T](coll.asScala.toSet)
  implicit def createConstraintCollection[T <: AnyRef](coll: mutable.LinkedHashSet[Constraint[T]]): ConstraintCollection[T, T] = new ConstraintCollection[T, T](coll.toSet)
}

class ConstraintCollection[T, U](coll: Set[Constraint[U]]) {
  def ForAll = new ForAll[T, U](coll)
  def Exists = new AtLeast[T, U](coll, 1)
  def AtLeast(k: Int) = new AtLeast[T, U](coll, k)
  def AtMost(k: Int) = new AtMost[T, U](coll, k)
  def Exactly(k: Int) = new Exactly[T, U](coll, k)
}

class ConstraintObjWrapper[T](coll: Seq[T]) {
  def ForAll[U](sensors: T => Constraint[U])(implicit tag: ClassTag[T]): ForAll[T, U] = {
    new ForAll[T, U](coll.map(sensors).toSet)
  }
  def Exists[U](sensors: T => Constraint[U])(implicit tag: ClassTag[T]): AtLeast[T, U] = {
    new AtLeast[T, U](coll.map(sensors).toSet, 1)
  }
  def AtLeast[U](k: Int)(sensors: T => Constraint[U])(implicit tag: ClassTag[T]): AtLeast[T, U] = {
    new AtLeast[T, U](coll.map(sensors).toSet, k)
  }
  def AtMost[U](k: Int)(sensors: T => Constraint[U])(implicit tag: ClassTag[T]): AtMost[T, U] = {
    new AtMost[T, U](coll.map(sensors).toSet, k)
  }
  def Exactly[U](k: Int)(sensors: T => Constraint[U])(implicit tag: ClassTag[T]): Exactly[T, U] = {
    new Exactly[T, U](coll.map(sensors).toSet, k)
  }
}

sealed trait Constraint[T] {
  def and[U](cons: Constraint[U]) = {
    new PairConjunction[T, U](this, cons)
  }

  def or[U](cons: Constraint[U]) = {
    new PairDisjunction[T, U](this, cons)
  }

  def implies[U](q: Constraint[U]): PairConjunction[T, U] = {
    // p --> q can be modelled as p or not(q)
    PairConjunction[T, U](this, q.negate)
  }
  def ==>[U](q: Constraint[U]): PairConjunction[T, U] = implies(q)

  def negate: Constraint[T]
  def unary_! = negate
}

// zero-th order constraints
sealed trait PropositionalConstraint[T] extends Constraint[T] {
  def estimator: LBJLearnerEquivalent
}

case class InstanceWrapper[T](instance: T, estimator: LBJLearnerEquivalent)

case class PropositionalEqualityConstraint[T](
  estimator: LBJLearnerEquivalent,
  instanceOpt: Option[T],
  equalityValOpt: Option[String],
  inequalityValOpt: Option[String]
) extends PropositionalConstraint[T] {
  def is(targetValue: String): PropositionalEqualityConstraint[T] = new PropositionalEqualityConstraint[T](estimator, instanceOpt, Some(targetValue), None)
  def isTrue = is("true")
  def isFalse = is("false")
  def isNot(targetValue: String): PropositionalEqualityConstraint[T] = new PropositionalEqualityConstraint[T](estimator, instanceOpt, None, Some(targetValue))
  def negate: Constraint[T] = {
    if (equalityValOpt.isDefined) {
      new PropositionalEqualityConstraint[T](estimator, instanceOpt, None, equalityValOpt)
    } else {
      new PropositionalEqualityConstraint[T](estimator, instanceOpt, inequalityValOpt, None)
    }
  }
  def isOneOf(values: Traversable[String]): AtLeast[T, T] = {
    val equalityConst = values.map { v => new PropositionalEqualityConstraint[T](estimator, instanceOpt, Some(v), None) }
    new AtLeast[T, T](equalityConst.toSet, 1)
  }

  def isOneOf(values: String*): AtLeast[T, T] = {
    isOneOf(values.toArray)
  }
}

// the two estimators should have the same prediction on the given instance
case class EstimatorPairEqualityConstraint[T](
  estimator1: LBJLearnerEquivalent,
  estimator2Opt: Option[LBJLearnerEquivalent],
  instance: T,
  equalsOpt: Option[Boolean]
) extends PropositionalConstraint[T] {
  def equalsTo(estimator2: LBJLearnerEquivalent) = new EstimatorPairEqualityConstraint[T](
    this.estimator1,
    Some(estimator2),
    this.instance,
    Some(true)
  )
  def differentFrom(estimator2: LBJLearnerEquivalent) = new EstimatorPairEqualityConstraint[T](
    this.estimator1,
    Some(estimator2),
    this.instance,
    Some(false)
  )
  override def estimator: LBJLearnerEquivalent = ???
  override def negate: Constraint[T] = new EstimatorPairEqualityConstraint(
    estimator1, estimator2Opt, instance, Some(!equalsOpt.get)
  )
}

case class InstancePairEqualityConstraint[T](
  estimator: LBJLearnerEquivalent,
  instance1: T,
  instance2Opt: Option[T],
  equalsOpt: Option[Boolean]
) extends PropositionalConstraint[T] {
  def equalsTo(instance2: T) = new InstancePairEqualityConstraint[T](
    this.estimator,
    this.instance1,
    Some(instance2),
    Some(true)
  )
  def differentFrom(instance2: T) = new InstancePairEqualityConstraint[T](
    this.estimator,
    this.instance1,
    Some(instance2),
    Some(false)
  )
  override def negate: Constraint[T] = new InstancePairEqualityConstraint(
    estimator, instance1, instance2Opt, Some(!equalsOpt.get)
  )
}

case class PairConjunction[T, U](c1: Constraint[T], c2: Constraint[U]) extends Constraint[T] {
  def negate: Constraint[T] = new PairDisjunction[T, U](c1.negate, c2.negate)
}

case class PairDisjunction[T, U](c1: Constraint[T], c2: Constraint[U]) extends Constraint[T] {
  def negate: Constraint[T] = new PairConjunction[T, U](c1.negate, c2.negate)
}

case class ForAll[T, U](constraints: Set[Constraint[U]]) extends Constraint[T] {
  def negate: Constraint[T] = new ForAll[T, U](constraints.map(_.negate))
}

case class AtLeast[T, U](constraints: Set[Constraint[U]], k: Int) extends Constraint[T] {
  def negate: Constraint[T] = new AtMost[T, U](constraints, k)
}

case class AtMost[T, U](constraints: Set[Constraint[U]], k: Int) extends Constraint[T] {
  def negate: Constraint[T] = new AtLeast[T, U](constraints, k)
}

case class Exactly[T, U](constraints: Set[Constraint[U]], k: Int) extends Constraint[T] {
  def negate: Constraint[T] = new Exactly[T, U](constraints.map(_.negate), k)
}

case class Implication[T, U](p: Constraint[T], q: Constraint[U]) extends Constraint[T] {
  def negate: Constraint[T] = Implication[T, U](p, q.negate)
}

case class Negation[T](p: Constraint[T]) extends Constraint[T] {
  // negation of negation
  def negate: Constraint[T] = p
}
