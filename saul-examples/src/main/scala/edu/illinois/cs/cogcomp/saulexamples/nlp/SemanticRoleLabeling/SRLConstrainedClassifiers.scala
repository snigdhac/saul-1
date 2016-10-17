/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Relation, TextAnnotation }
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLClassifiers.{ argumentTypeLearner, argumentXuIdentifierGivenApredicate }
import edu.illinois.cs.cogcomp.saulexamples.nlp.SemanticRoleLabeling.SRLConstraints._

object SRLConstrainedClassifiers {
  import SRLApps.srlDataModelObject._

  object argTypeConstraintClassifier extends ConstrainedClassifier[Relation, TextAnnotation] {
    def subjectTo = r_and_c_args
    override val solverType = OJAlgo
    override lazy val estimator = argumentTypeLearner
    override val pathToHead = Some(-sentencesToRelations)
  }

  object arg_Is_TypeConstraintClassifier extends ConstrainedClassifier[Relation, Relation] {
    def subjectTo = arg_IdentifierClassifier_Constraint
    override val solverType = OJAlgo
    override lazy val estimator = argumentTypeLearner
  }

  object arg_IdentifyConstraintClassifier extends ConstrainedClassifier[Relation, Relation] {
    def subjectTo = arg_IdentifierClassifier_Constraint
    override val solverType = OJAlgo
    override lazy val estimator = argumentXuIdentifierGivenApredicate
  }
}

