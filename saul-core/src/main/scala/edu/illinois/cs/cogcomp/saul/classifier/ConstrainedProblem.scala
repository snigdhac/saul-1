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
        estimator match {
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

  def cacheKey[U](u: U): String = u.toString //+ u.hashCode()

  def getInstancesInvolvedInProblem: Option[Set[_]] = {
    constraintsOpt.map { constraint => getInstancesInvolved(constraint) }
  }

  def getInstancesInvolved(constraint: SaulConstraint[_]): Set[_] = {
    constraint match {
      case c: SaulPropositionalEqualityConstraint[_] =>
        Set(c.instanceOpt.get)
      case c: SaulPairConjunction[_, Any] =>
        getInstancesInvolved(c.c1) ++ getInstancesInvolved(c.c2)
      case c: SaulPairDisjunction[_, Any] =>
        getInstancesInvolved(c.c1) ++ getInstancesInvolved(c.c2)
      case c: SaulNegation[_] =>
        getInstancesInvolved(c.p)
      case c: SaulAtLeast[_, Any] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: SaulAtMost[_, Any] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: SaulForAll[_, Any] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
      case c: SaulExactly[_, Any] =>
        c.constraints.foldRight(Set[Any]()) {
          case (singleConstraint, ins) =>
            ins union getInstancesInvolved(singleConstraint).asInstanceOf[Set[Any]]
        }
    }
  }

  private def build(head: HEAD, t: T)(implicit d: DummyImplicit): String = {
    val mainCacheKey = getInstancesInvolvedInProblem.map(cacheKey(_)).toSeq.sorted.mkString("*") + estimator.toString + constraintsOpt
    logger.info("***************** mainCacheKey = " + mainCacheKey)
    val resultOpt = cachedResults.get(mainCacheKey)
    resultOpt match {
      case Some(estimatorPredictions) =>
        logger.info(s" *********** Reading the results from cache . . . ")
        logger.info(s"Cache size " + cachedResults.size)
        logger.info(s"cachedResults: " + cachedResults)
        val labelsPerInstances = estimatorPredictions(estimator)
        println("labelsPerInstances = " + labelsPerInstances)
        require(labelsPerInstances.contains(cacheKey(t)), s"Does not contain the cache key for ${cacheKey(t)}")
        labelsPerInstances.get(cacheKey(t)).get
      case None =>
        logger.info(s" *********** Inference $mainCacheKey has not been cached; running inference . . . ")

        // create a new solver instance
        val solver = getSolverInstance
        solver.setMaximize(optimizationType == Max)

        // populate the instances connected to head
        val candidates = getCandidates(head)
        //    println("*** candidates = " + candidates)
        addVariablesToInferenceProblem(candidates, estimator, solver)

        //    println("estimatorToSolverLabelMap = " + estimatorToSolverLabelMap)

        // populate the constraints and relevant variables
        //println("constraintsOpt = ")
        //    println(constraintsOpt)
        //    println(constraintsOpt)
        constraintsOpt.foreach {
          case constraints =>
            println("constraints = ")
            println(constraints)
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
        if (!solver.isSolved) {
          println(" /////// NOT SOLVED /////// ")
        }

        val estimatorSpecificMap = estimatorToSolverLabelMap.get(estimator).get.asInstanceOf[mutable.Map[T, Seq[(Int, String)]]]

        println("***** estimatorToSolverLabelMap")
        println(estimatorToSolverLabelMap)

        // println(" ----- after solving it ------ ")
        //        (0 to 11).foreach { int =>
        //          println("int = " + int)
        //          println("solver.getIntegerValue(int) = " + solver.getIntegerValue(int))
        //          println("-------")
        //        }

        val labelsPerEstimatorPerInstance = estimatorToSolverLabelMap.mapValues { instanceLabelMap =>
          instanceLabelMap.map {
            case (c, indexLabelPairs) =>
              val instanceKey = cacheKey(c)
              val predictedLabel = indexLabelPairs.collectFirst {
                case (ind, label) if solver.getIntegerValue(ind) == 1.0 => label
              }.get
              instanceKey -> predictedLabel
          }
        }

        cachedResults.put(mainCacheKey, labelsPerEstimatorPerInstance)

        //println("# of candidates: " + candidates.length)
        //println("length of instanceLabelVarMap: " + estimatorToSolverLabelMap.size)
        //println("length of instanceLabelVarMap: " + estimatorToSolverLabelMap.get(estimator).get.size)
        println("***** estimatorSpecificMap = ")
        println(estimatorSpecificMap)
        estimatorSpecificMap.get(t) match {
          case Some(indexLabelPairs) =>
            val values = indexLabelPairs.map {
              case (ind, label) =>
                println(s"ind=$ind / label=$label / solver.getIntegerValue(ind)=${solver.getIntegerValue(ind)}")
                solver.getIntegerValue(ind)
            }
            assert(values.sum == 1, "exactly one label should be active.")

            indexLabelPairs.collectFirst {
              case (ind, label) if solver.getIntegerValue(ind) == 1.0 => label
            }.get
          case None => throw new Exception("instance is not cached ... weird! :-/ ")
        }
    }
  }

  //def solve(): Boolean = ??? /// solver.solve()

  /** Test Constrained Classifier with automatically derived test instances.
    *
    * @return Seq of ???
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
    println()
    println("size of test data = " + testData.size)
    val testReader = new IterableToLBJavaParser[T](if (testData == null) deriveTestInstances else testData)
    testReader.reset()
    println("testReader.data.size = " + testReader.data.size)

    val tester: TestDiscrete = new TestDiscrete()
    TestWithStorage.test(tester, estimator.classifier, estimator.getLabeler, testReader, outFile, outputGranularity, exclude)
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

object ConstrainedProblem {
  // a small number used in creation of exclusive inequalities
  private val epsilon = 0.01

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

        //        println("****** c.instanceOpt.get = " + c.instanceOpt.get)
        val (ilpIndices, labels) = estimatorScoresMap.get(c.instanceOpt.get) match {
          case Some(ilpIndexLabelPairs) => ilpIndexLabelPairs.unzip
          case None =>
            val confidenceScores = c.estimator.classifier.scores(c).toArray.map(_.score)
            val labels = c.estimator.classifier.scores(c.instanceOpt.get).toArray.map(_.value)
            val indicesPerLabels = solver.addDiscreteVariable(confidenceScores)
            estimatorScoresMap += (c.instanceOpt.get -> indicesPerLabels.zip(labels))
            println(" ---> adding: estimatorScoresMap = " + estimatorScoresMap)
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
      case c: SaulPairConjunction[V, Any] =>
        val InequalitySystem1 = processConstraints(c.c1, solver)
        val InequalitySystem2 = processConstraints(c.c2, solver)

        // conjunction is simple; you just include all the inequalities
        InequalitySystem1 union InequalitySystem2
      case c: SaulPairDisjunction[V, Any] =>
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
      case c: SaulNegation[V] =>
        // change the signs of the coefficients
        val InequalitySystemToBeNegated = processConstraints(c.p, solver)
        InequalitySystemToBeNegated.map { in =>
          val minusA = in.a.map(-_)
          val minusB = -in.b + epsilon
          ILPInequalityGEQ(minusA, in.x, minusB)
        }
      case c: SaulAtLeast[V, Any] =>
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
      case c: SaulAtMost[V, Any] =>
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
      case c: SaulExactly[V, Any] =>
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
      //      println("-- instance = " + c)
      val confidenceScores = estimator.classifier.scores(c).toArray.map(_.score)
      val labels = estimator.classifier.scores(c).toArray.map(_.value)
      println("labels = " + labels.toSeq)
      val instanceIndexPerLabel = solver.addDiscreteVariable(confidenceScores)
      println("instanceIndexPerLabel = " + instanceIndexPerLabel.toSeq)
      println(" ---> adding: estimatorScoresMap2 = " + estimatorScoresMap)
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

  /** Contains cache of problems already solved. The key is the head object, which maps to instances and their
    * predicted values in the output of inference
    */
  val cachedResults = mutable.Map[String, Map[LBJLearnerEquivalent, Map[String, String]]]()

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
  def Exactly[U](k: Int)(sensors: T => SaulConstraint[U])(implicit tag: ClassTag[T]): SaulExactly[T, U] = {
    new SaulExactly[T, U](coll.map(sensors).toSet, k)
  }
}

sealed trait SaulConstraint[T] {
  def and4[U](cons: SaulConstraint[U]) = {
    new SaulPairConjunction[T, U](this, cons)
  }

  def or4[U](cons: SaulConstraint[U]) = {
    new SaulPairDisjunction[T, U](this, cons)
  }

  def implies[U](q: SaulConstraint[U]): SaulPairConjunction[T, U] = {
    // p --> q can be modelled as p or not(q)
    SaulPairConjunction[T, U](this, q.negate)
  }

  def ====>[U](q: SaulConstraint[U]): SaulPairConjunction[T, U] = implies(q)

  def negate: SaulConstraint[T]
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
  def negate: SaulConstraint[T] = {
    if (equalityValOpt.isDefined) {
      new SaulPropositionalEqualityConstraint[T](estimator, instanceOpt, None, equalityValOpt)
    } else {
      new SaulPropositionalEqualityConstraint[T](estimator, instanceOpt, inequalityValOpt, None)
    }
  }
}

case class SaulPairConjunction[T, U](c1: SaulConstraint[T], c2: SaulConstraint[U]) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulPairDisjunction[T, U](c1.negate, c2.negate)
}

case class SaulPairDisjunction[T, U](c1: SaulConstraint[T], c2: SaulConstraint[U]) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulPairConjunction[T, U](c1.negate, c2.negate)
}

case class SaulForAll[T, U](constraints: Set[SaulConstraint[U]]) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulForAll[T, U](constraints.map(_.negate))
}

case class SaulAtLeast[T, U](constraints: Set[SaulConstraint[U]], k: Int) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulAtMost[T, U](constraints, constraints.size - k)
}

case class SaulAtMost[T, U](constraints: Set[SaulConstraint[U]], k: Int) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulAtLeast[T, U](constraints, constraints.size - k)
}

case class SaulExactly[T, U](constraints: Set[SaulConstraint[U]], k: Int) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = new SaulExactly[T, U](constraints.map(_.negate), k)
}

case class SaulImplication[T, U](p: SaulConstraint[T], q: SaulConstraint[U]) extends SaulConstraint[T] {
  def negate: SaulConstraint[T] = SaulImplication[T, U](p, q.negate)
}

case class SaulNegation[T](p: SaulConstraint[T]) extends SaulConstraint[T] {
  // negation of negation
  def negate: SaulConstraint[T] = p
}
