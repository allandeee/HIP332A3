/**
  * Hipster cellular automaton language. Miscellaneous utilities.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

object Utils {

  // Regular expressions for processing numerical constants.
  lazy val intRegex =
    "0[oO]([0-8]*)|0[bB]([01]+)|0[xX]([0-9A-Fa-f]+)|[0-9]+".r

  lazy val floatRegex =
    "[0-9]+(\\.[0-9]+([eE][-+]?[0-9]+)?|[eE][-+]?[0-9]+)".r

  /** 
    * Convert strings in decimal / hexadecimal / octal / binary
    * notation to integers. Returns either the converted integer
    * or an error message, so this can be used with the
    * [[org.bitbucket.inkytonik.kiama.parsers.Parser.wrap]]
    * combinator.
    */
  def parseToInt(s : String): Either[Int,String] = s match {
    case intRegex(a,b,c) =>
      try {
        if (a != null) Left(Integer.parseUnsignedInt(a,8))
        else if (b != null) Left(Integer.parseUnsignedInt(b,2))
        else if (c != null) Left(Integer.parseUnsignedInt(c,16))
        else Left(Integer.parseInt(s,10))
      } catch {
        case ex : NumberFormatException =>
          Right("integer will not fit into 32 bits")
      }
    case _ =>
      Right("integer constant expected")
  }

  /**
    * Convert strings in floating point notation to a double. 
    * Returns either the converted double or an error message, 
    * so this can be used with the
    * [[org.bitbucket.inkytonik.kiama.parsers.Parser.wrap]]
    * combinator.
    */
  def parseToDouble(s:String): Either[Double,String] = s match {
    case floatRegex(_*) =>
        val d = s.toDouble
        if (d.isInfinity)
           Right("floating point number is infinite")
        else
          Left(s.toDouble)
    case _ =>
      Right("floating point constant expected")
  }
}


