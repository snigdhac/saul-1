/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation._
import edu.illinois.cs.cogcomp.saul.classifier.infer.Constraint._
import edu.illinois.cs.cogcomp.saulexamples.data.XuPalmerCandidateGenerator
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLApps.srlDataModelObject._
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLClassifiers.{ argumentTypeLearner, argumentXuIdentifierGivenPredicate, predicateClassifier }

import scala.collection.JavaConversions._

object SRLConstraints {
  def noOverlap = sentences.ForEach { x: TextAnnotation =>
    (sentences(x) ~> sentencesToRelations ~> relationsToPredicates).ForAll { y =>
      val argCandList = XuPalmerCandidateGenerator.generateCandidates(y, (sentences(y.getTextAnnotation) ~> sentencesToStringTree).head).
        map(y => new Relation("candidate", y.cloneForNewView(y.getViewName), y.cloneForNewView(y.getViewName), 0.0))
      x.getView(ViewNames.TOKENS).getConstituents.ForAll {
        t: Constituent =>
          val contains = argCandList.filter(_.getTarget.doesConstituentCover(t))
          contains.AtMost(1) { p: Relation => argumentTypeLearner on p is "candidate" }
      }
    }
  }

  def arg_IdentifierClassifier_Constraint = relations.ForEach { x: Relation =>
    (argumentXuIdentifierGivenPredicate on x isFalse) ==> (argumentTypeLearner on x is "candidate")
  }

  def predArg_IdentifierClassifier_Constraint = relations.ForEach { x: Relation =>
    (predicateClassifier on x.getSource isTrue) and (argumentXuIdentifierGivenPredicate on x isTrue) ==>
      (argumentTypeLearner on x isNot "candidate")
  }

  /** constraint for reference to an actual argument/adjunct of type arg */
  def rArgConstraint(x: TextAnnotation) = {
    val values = Array("R-A1", "R-A2", "R-A3", "R-A4", "R-AA", "R-AM-ADV", "R-AM-CAU", "R-AM-EXT", "R-AM-LOC", "R-AM-MNR", "R-AM-PNC")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      r: Relation <- argCandList
      i <- values.indices
    } yield ((argumentTypeLearner on r) is values(i)) ==>
      argCandList.filterNot(x => x.equals(r)).Exists {
        k: Relation => (argumentTypeLearner on k) is values(i).substring(2)
      }
    constraints.ForAll
  }

  /** constraint for continuity of an argument/adjunct of type arg */
  def cArgConstraint(x: TextAnnotation) = {
    val values = Array("C-A1", "C-A2", "C-A3", "C-A4", "C-A5", "C-AM-DIR", "C-AM-LOC", "C-AM-MNR", "C-AM-NEG", "C-AM-PNC")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      sortedCandidates = argCandList.sortBy(x => x.getTarget.getStartSpan)
      (t, ind) <- sortedCandidates.zipWithIndex.drop(1)
      i <- values.indices
      labelOnT = (argumentTypeLearner on t) is values(i)
      labelsIsValid = sortedCandidates.subList(0, ind).Exists {
        k: Relation => (argumentTypeLearner on k) is values(i).substring(2)
      }
    } yield labelOnT ==> labelsIsValid
    constraints.ForAll
  }

  /** the label of the classifier should be valid */
  def legalArgumentsConstraint(x: TextAnnotation) = {
    // these are the labels that are not used in the 'argumentTypeLearner' classifier
    val excludedLabels = Set("R-AM-NEG", "R-AM-MOD", "<null>", "R-AM-DIS", "R-AM-REC", "R-AM-PRD", "C-AM-REC",
      "C-AM-PRD", "R-AM-DIR", "C-AM-MOD", "AM", "R-AM", "C-AM")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      argLegalList = legalArguments(y).toSet diff excludedLabels
      z <- argCandList
    } yield argLegalList.Exists { t: String => argumentTypeLearner on z is t } or
      (argumentTypeLearner on z is "candidate")
    constraints.ForAll
  }

  // Predicates have at most one argument of each type i.e. there shouldn't be any two arguments with the same type for each predicate
  def noInconsistentPredicateLabels(x: TextAnnotation) = {
    val values = Array("A0", "A1", "A2", "A3", "A4", "A5", "AA")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      idx1 <- 0 until argCandList.size - 1
      idx2 <- idx1 + 1 until argCandList.size
      predictionIsValid = (argumentTypeLearner on argCandList.get(idx1)) isOneOf values
      haveSameLabels = (argumentTypeLearner on argCandList.get(idx1)) equalsTo argCandList.get(idx2)
    } yield predictionIsValid ==> haveSameLabels
    constraints.ForAll
  }

  def allPredicateArgumentConstraints = sentences.ForEach { x: TextAnnotation =>
    rArgConstraint(x) and cArgConstraint(x) and legalArgumentsConstraint(x) and noInconsistentPredicateLabels(x)
  }
}

