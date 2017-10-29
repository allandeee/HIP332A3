/**
  * Hipster cellular automaton language.
  * Some utility functions related to attribution.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama._
import attribution.Attribution

/**
  * Attribute definitions for Hipster name analysis.
  */
trait AttributionUtils extends Attribution {
  import HipsterTree._
  import scala.reflect.ClassTag

  def tree : HipsterTree

    // A few utility functions.

  /**
    * Test to see if the previous sibling node to a given node 
    * has a specified type. Sometimes of use in pattern guards.
    */
  def prevIsOfType[T](n : HipsterNode)(implicit tag : ClassTag[T]) =
    n match {
      case tree.prev(_ : T) => true
      case _ => false
    }

  /**
    * Test to see if the next sibling node to a given node 
    * has a specified type. Sometimes of use in pattern guards.
    */
  def nextIsOfType[T](n : HipsterNode)(implicit tag : ClassTag[T]) =
    n match {
      case tree.next(_ : T) => true
      case _ => false
    }

  /**
    * Test to see if the parent node of a given node 
    * has a specified type. Sometimes of use in pattern guards.
    */
  def parentIsOfType[T](n : HipsterNode)(implicit tag : ClassTag[T]) =
    n match {
      case tree.parent(_ : T) => true
      case _ => false
    }
}
