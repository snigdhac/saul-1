/** This software is released under the University of Illinois/Research and Academic Use License. See
  * the LICENSE file in the root folder for details. Copyright (c) 2016
  *
  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
  * http://cogcomp.cs.illinois.edu/
  */
package edu.illinois.cs.cogcomp.saulexamples

import edu.illinois.cs.cogcomp.saulexamples.setcover._
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.JavaConversions._

class SetCoverTest extends FlatSpec with Matchers {
  def prefix(t: String = "test") = s"../saul-examples/src/$t/resources/SetCover/"

  "SetCover " should " be solved correctly for example.txt " in {
    SetCoverSolverDataModel.clearInstances
    val citiesInstance = new City(prefix("main") + "example.txt")
    val neighborhoodInstances = citiesInstance.getNeighborhoods.toList

    SetCoverSolverDataModel.cities populate List(citiesInstance)
    SetCoverSolverDataModel.neighborhoods populate neighborhoodInstances
    SetCoverSolverDataModel.cityContainsNeighborhoods.populateWith(_ == _.getParentCity)

    val neighborhoodLabels = Map(1 -> true, 2 -> false, 3 -> false, 4 -> false, 5 -> false, 6 -> true,
      7 -> false, 8 -> false, 9 -> false)

    citiesInstance.getNeighborhoods.forall { n =>
      ConstrainedContainsStation(n) == neighborhoodLabels(n.getNumber).toString
    } should be(true)
    val neighborhoodOutput = "List(neighborhood #1, neighborhood #2, neighborhood #3, neighborhood #4, neighborhood #5, neighborhood #6, neighborhood #7, neighborhood #8, neighborhood #9)"
    ConstrainedContainsStation.getCandidates(citiesInstance).toList.toString should be(neighborhoodOutput)
    SetCoverSolverDataModel.cityContainsNeighborhoods(citiesInstance).toList.sorted.toString should be(neighborhoodOutput)
  }

  "SetCover " should " be solved correctly for example2.txt " in {
    SetCoverSolverDataModel.clearInstances
    val citiesInstance = new City(prefix() + "example2.txt")
    val neighborhoodInstances = citiesInstance.getNeighborhoods.toList

    SetCoverSolverDataModel.cities populate List(citiesInstance)
    SetCoverSolverDataModel.neighborhoods populate neighborhoodInstances
    SetCoverSolverDataModel.cityContainsNeighborhoods.populateWith(_ == _.getParentCity)

    val neighborhoodLabels = Map(1 -> true, 2 -> true, 3 -> false, 4 -> false,
      5 -> false, 6 -> false, 7 -> false, 8 -> false)

    citiesInstance.getNeighborhoods.forall { n =>
      ConstrainedContainsStation(n) == neighborhoodLabels(n.getNumber).toString
    } should be(true)
  }

  "SetCover " should " be solved correctly for example3.txt " in {
    SetCoverSolverDataModel.clearInstances
    val citiesInstance = new City(prefix() + "example3.txt")
    val neighborhoodInstances = citiesInstance.getNeighborhoods.toList

    SetCoverSolverDataModel.cities populate List(citiesInstance)
    SetCoverSolverDataModel.neighborhoods populate neighborhoodInstances
    SetCoverSolverDataModel.cityContainsNeighborhoods.populateWith(_ == _.getParentCity)

    val neighborhoodLabels = Map(1 -> true, 2 -> false, 3 -> false)

    citiesInstance.getNeighborhoods.forall { n =>
      ConstrainedContainsStation(n) == neighborhoodLabels(n.getNumber).toString
    } should be(true)
  }
}
