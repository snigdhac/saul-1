/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.nlp.EntityRelation

import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import edu.illinois.cs.cogcomp.saulexamples.EntityMentionRelation.datastruct.{ ConllRawToken, ConllRelation }

object EntityRelationConstrainedClassifiers {
  object OrgConstrainedClassifier extends ConstrainedClassifier[ConllRawToken, ConllRelation] {
    override lazy val estimator = EntityRelationClassifiers.OrganizationClassifier
    override def pathToHead = Some(-EntityRelationDataModel.pairTo2ndArg)
    override def constraintsOpt = Some(EntityRelationConstraints.relationArgumentConstraints)
    override def filter(t: ConllRawToken, h: ConllRelation): Boolean = t.wordId == h.wordId2
    override def solverType = OJAlgo
  }

  object PerConstrainedClassifier extends ConstrainedClassifier[ConllRawToken, ConllRelation] {
    override lazy val estimator = EntityRelationClassifiers.PersonClassifier
    override def pathToHead = Some(-EntityRelationDataModel.pairTo1stArg)
    override def constraintsOpt = Some(EntityRelationConstraints.relationArgumentConstraints)
    override def filter(t: ConllRawToken, h: ConllRelation): Boolean = t.wordId == h.wordId1
    override def solverType = OJAlgo
  }

  object LocConstrainedClassifier extends ConstrainedClassifier[ConllRawToken, ConllRelation] {
    override lazy val estimator = EntityRelationClassifiers.LocationClassifier
    override def pathToHead = Some(-EntityRelationDataModel.pairTo2ndArg)
    override def constraintsOpt = Some(EntityRelationConstraints.relationArgumentConstraints)
    override def filter(t: ConllRawToken, h: ConllRelation): Boolean = t.wordId == h.wordId2
    override def solverType = OJAlgo
  }

  object WorksForRelationConstrainedClassifier extends ConstrainedClassifier[ConllRawToken, ConllRelation] {
    override lazy val estimator = EntityRelationClassifiers.WorksForClassifier
    override def constraintsOpt = Some(EntityRelationConstraints.relationArgumentConstraints)
    override def solverType = OJAlgo
  }

  object LivesInRelationConstrainedClassifier extends ConstrainedClassifier[ConllRawToken, ConllRelation] {
    override lazy val estimator = EntityRelationClassifiers.LivesInClassifier
    override def constraintsOpt = Some(EntityRelationConstraints.relationArgumentConstraints)
    override def solverType = OJAlgo
  }
}

