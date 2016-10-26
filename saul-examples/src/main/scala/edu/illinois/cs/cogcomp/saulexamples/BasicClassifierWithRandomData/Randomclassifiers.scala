package edu.illinois.cs.cogcomp.saulexamples.BasicClassifierWithRandomData

import edu.illinois.cs.cogcomp.lbjava.learn.SparseNetworkLearner
import edu.illinois.cs.cogcomp.saul.classifier.Learnable

object RandomClassifiers {
  import RandomDataModel._
  object BinaryClassifier extends Learnable[String](randomNode) {
    def label = randomLabel
    override def feature = using(randomProperty)
    override lazy val classifier = new SparseNetworkLearner()
    override val useCache = true
  }
}
