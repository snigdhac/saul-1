/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples.setcover

import edu.illinois.cs.cogcomp.saul.util.Logging

import scala.collection.JavaConversions._

object SetCoverApp extends Logging {
  val cityInstances = new City("src/main/resources/SetCover/example.txt")
  val neighborhoodInstances = cityInstances.getNeighborhoods.toList

  def main(args: Array[String]) {
    println("in main: allowable values: " + new ContainsStation().allowableValues.toSeq)
    SetCoverSolverDataModel.cities populate List(cityInstances)
    SetCoverSolverDataModel.neighborhoods populate neighborhoodInstances
    def getParentCity = (n: Neighborhood) => n.getParentCity
    SetCoverSolverDataModel.cityContainsNeighborhoods.populateWith((c: City, n: Neighborhood) => n.getParentCity == c)
    cityInstances.getNeighborhoods.foreach {
      n => logger.info(n.getNumber + ": " + ConstrainedContainsStation(n))
    }
  }
}