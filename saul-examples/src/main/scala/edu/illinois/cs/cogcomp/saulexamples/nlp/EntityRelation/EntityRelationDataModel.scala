/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.nlp.EntityRelation

import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saulexamples.EntityMentionRelation.datastruct.{ ConllRawSentence, ConllRawToken, ConllRelation }
import edu.illinois.cs.cogcomp.saulexamples.nlp.EntityRelation.EntityRelationClassifiers._
import edu.illinois.cs.cogcomp.saulexamples.nlp.EntityRelation.EntityRelationSensors._

import scala.collection.JavaConverters._

object EntityRelationDataModel extends DataModel {

  /** Nodes & Edges */
  val tokens = node[ConllRawToken]
  val sentences = node[ConllRawSentence]
  val pairs = node[ConllRelation]

  val sentenceToToken = edge(sentences, tokens)
  val sentencesToPairs = edge(sentences, pairs)
  val pairTo1stArg = edge(pairs, tokens)
  val pairTo2ndArg = edge(pairs, tokens)
  val tokenToPair = edge(tokens, pairs)

  sentenceToToken.addSensor(sentenceToTokens_GeneratingSensor _)
  sentencesToPairs.addSensor(sentenceToRelation_GeneratingSensor _)
  pairTo1stArg.addSensor(relationToFirstArg_MatchingSensor _)
  pairTo2ndArg.addSensor(relationToSecondArg_MatchingSensor _)

  /** Properties */
  val pos = property(tokens) {
    t: ConllRawToken => t.POS
  }

  val word = property(tokens) {
    t: ConllRawToken => t.getWords(false).toList
  }

  val phrase = property(tokens) {
    t: ConllRawToken => t.phrase
  }

  val tokenSurface = property(tokens) {
    t: ConllRawToken => t.getWords(false).mkString(" ")
  }

  val containsSubPhraseMent = property(tokens) {
    t: ConllRawToken => t.getWords(false).exists(_.contains("ment")).toString
  }

  val containsSubPhraseIng = property(tokens) {
    t: ConllRawToken => t.getWords(false).exists(_.contains("ing")).toString
  }

  val containsInCityList = property(tokens, cache = true) {
    t: ConllRawToken => cityGazetSensor.isContainedIn(t)
  }

  val containsInPersonList = property(tokens, cache = true) {
    t: ConllRawToken => personGazetSensor.containsAny(t)
  }

  val wordLen = property(tokens) {
    t: ConllRawToken => t.getLength
  }

  val relFeature = property(pairs) {
    token: ConllRelation =>
      "w1-word-" + token.e1.phrase :: "w2-word-" + token.e2.phrase ::
        "w1-pos-" + token.e1.POS :: "w2-pos-" + token.e2.POS ::
        "w1-city-" + cityGazetSensor.isContainedIn(token.e1) :: "w2-city-" + cityGazetSensor.isContainedIn(token.e2) ::
        "w1-per-" + personGazetSensor.containsAny(token.e1) :: "w2-per-" + personGazetSensor.containsAny(token.e2) ::
        "w1-ment-" + token.e1.getWords(false).exists(_.contains("ing")) ::
        "w2-ment-" + token.e2.getWords(false).exists(_.contains("ing")) ::
        "w1-ing-" + token.e1.getWords(false).exists(_.contains("ing")) ::
        "w2-ing-" + token.e2.getWords(false).exists(_.contains("ing")) ::
        Nil
  }

  val tokensWithinWindowPos = property(tokens) { t: ConllRawToken =>
    val allSentenceTokens = t.s.sentTokens.asScala
    val indexOpt = allSentenceTokens.zipWithIndex.collectFirst { case (item, idx) if item == t => idx }
    require(indexOpt.isDefined, "ERROR: the token not found in the sentence!")
    val minInd = Math.max(indexOpt.get - 2, 0)
    val maxInd = Math.min(indexOpt.get + 2, allSentenceTokens.length - 1)
    val tokensInWindow = allSentenceTokens.slice(minInd, maxInd)
    tokensInWindow.map(pos(_)).toList
  }

  val tokensWithinWindowRelPos = property(pairs) { rela: ConllRelation =>
    val e1 = rela.e1
    val e2 = rela.e2
    tokensWithinWindowPos(e1) ++ tokensWithinWindowPos(e2)
  }

  val entityPrediction = property[ConllRelation](pairs) {
    rel: ConllRelation =>
      List(
        "e1-org:" + OrganizationClassifier(rel.e1),
        "e1-per:" + PersonClassifier(rel.e1),
        "e1-loc:" + LocationClassifier(rel.e1),
        "e2-org:" + OrganizationClassifier(rel.e2),
        "e2-per:" + PersonClassifier(rel.e2),
        "e2-loc:" + LocationClassifier(rel.e2)
      )
  }

  /** Labeler Properties  */
  val entityType = property(tokens) {
    t: ConllRawToken => t.entType
  }

  val relationType = property(pairs) {
    r: ConllRelation => r.relType
  }

  def populateWithConll() = {
    sentences.populate(EntityRelationSensors.sentencesTrain)
    sentences.populate(EntityRelationSensors.sentencesTest, train = false)
  }
}
