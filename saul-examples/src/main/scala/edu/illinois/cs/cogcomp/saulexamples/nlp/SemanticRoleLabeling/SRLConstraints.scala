/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation._
import edu.illinois.cs.cogcomp.saul.classifier.SaulConstraint
import edu.illinois.cs.cogcomp.saulexamples.data.XuPalmerCandidateGenerator
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLApps.srlDataModelObject._
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLClassifiers.{ argumentTypeLearner, argumentXuIdentifierGivenApredicate, predicateClassifier }

import scala.collection.JavaConversions._
import SaulConstraint._

object SRLConstraints {
  def noOverlap = sentences.ForAll { x: TextAnnotation =>
    (sentences(x) ~> sentencesToRelations ~> relationsToPredicates).ForAll { y =>
      val argCandList = XuPalmerCandidateGenerator.generateCandidates(y, (sentences(y.getTextAnnotation) ~> sentencesToStringTree).head).
        map(y => new Relation("candidate", y.cloneForNewView(y.getViewName), y.cloneForNewView(y.getViewName), 0.0))
      x.getView(ViewNames.TOKENS).getConstituents.ForAll {
        t: Constituent =>
          val contains = argCandList.filter(_.getTarget.doesConstituentCover(t))
          contains.AtMost(1) { p: Relation => argumentTypeLearner on2 p is2 "candidate" }
      }
    }
  }

  def arg_IdentifierClassifier_Constraint = relations.ForAll { x: Relation =>
    (argumentXuIdentifierGivenApredicate on2 x isFalse2) ====> (argumentTypeLearner on2 x is2 "candidate")
  }

  def predArg_IdentifierClassifier_Constraint = relations.ForAll { x: Relation =>
    (predicateClassifier on2 x.getSource isTrue2) and4 (argumentXuIdentifierGivenApredicate on2 x isTrue2) ====>
      (argumentTypeLearner on2 x isNot2 "candidate")
  }

  def r_arg_Constraint(x: TextAnnotation) = {
    val values = Array("R-A1", "R-A2", "R-A3", "R-A4", "R-A5", "R-AA", "R-AM-ADV", "R-AM-CAU", "R-AM-EXT", "R-AM-LOC", "R-AM-MNR", "R-AM-PNC")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      t: Relation <- argCandList
      i <- values.indices
    } yield ((argumentTypeLearner on2 t) is2 values(i)) ====>
      argCandList.filterNot(x => x.equals(t)).Exists {
        k: Relation => (argumentTypeLearner on2 k) is2 values(i).substring(2)
      }
    constraints.ForAll
  }

  def c_arg_Constraint(x: TextAnnotation) = {
    val values = Array("C-A1", "C-A2", "C-A3", "C-A4", "C-A5", "C-AA", "C-AM-DIR", "C-AM-LOC", "C-AM-MNR", "C-AM-NEG", "C-AM-PNC")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      sortedCandidates = argCandList.sortBy(x => x.getTarget.getStartSpan)
      (t, ind) <- sortedCandidates.zipWithIndex.drop(1)
      i <- values.indices
      labelOnT = (argumentTypeLearner on2 t) is2 values(i)
      labelsIsValid = sortedCandidates.subList(0, ind).Exists {
        k: Relation => (argumentTypeLearner on2 k) is2 values(i).substring(2)
      }
    } yield labelOnT ====> labelsIsValid
    constraints.ForAll
  }

  def legal_arguments_Constraint(x: TextAnnotation) = {
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      argLegalList = legalArguments(y)
      z <- argCandList
    } yield argLegalList.Exists { t: String => argumentTypeLearner on2 z is2 t } or4
      (argumentTypeLearner on2 z is2 "candidate")
    constraints.ForAll
  }

  // Predicates have at most one argument of each type i.e. there shouldn't be any two arguments with the same type for each predicate
  def noDuplicate(x: TextAnnotation) = {
    val values = Array("A0", "A1", "A2", "A3", "A4", "A5", "AA")
    val constraints = for {
      y <- sentences(x) ~> sentencesToRelations ~> relationsToPredicates
      argCandList = (predicates(y) ~> -relationsToPredicates).toList
      t1 <- 0 until argCandList.size - 1
      t2 <- t1 + 1 until argCandList.size
      predictionOnT1IsValid = (argumentTypeLearner on2 argCandList.get(t1)) isOneOf values
      t1AndT2HaveSameLabels = (argumentTypeLearner on4 argCandList.get(t1)) equalsTo argCandList.get(t2)
    } yield predictionOnT1IsValid ====> t1AndT2HaveSameLabels
    constraints.ForAll
  }

  val r_and_c_args = sentences.ForAll { x: TextAnnotation =>
    r_arg_Constraint(x) and4 c_arg_Constraint(x) and4 legal_arguments_Constraint(x) and4 noDuplicate(x)
  }
}

