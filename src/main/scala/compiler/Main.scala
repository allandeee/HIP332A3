/**
  * Hipster cellular automaton language.
  * Main driver, parse command line, load and compile specified sources.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package org.bitbucket.dominicverity.hipster
package compiler

import org.bitbucket.inkytonik.kiama.util.Messaging
import org.bitbucket.inkytonik.kiama.util.PositionStore

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and print the source tree.
 */
object Main extends PositionStore with Messaging { 

  import java.io.FileNotFoundException
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
  import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
  import org.bitbucket.inkytonik.kiama.util.Messaging.message
  import org.bitbucket.inkytonik.kiama.util.FileSource

  def main (args : Array[String]) : Unit = {

    args.size match {

      // If there is exactly one command-line argument
      case 1 =>
        try {
          // Create a source for the argument file name
          val source = new FileSource (args (0))
          
          // Create a syntax analysis module
          val parsers = new SyntaxAnalysis (positions)

          // Parse the file
          parsers.parse (parsers.parser, source) match {
            // If it worked, we get a source tree
            case Success (sourcetree, _) =>

              // Pretty print the source tree
              println (layout (any (sourcetree)))

            // Parsing failed, so report it
            case res : NoSuccess =>
              val pos = res.next.position
              positions.setStart(res, pos)
              positions.setFinish(res, pos)
              val messages = message(res, res.message)
              println (formatMessages(messages))
          }
        } catch {
          case e : FileNotFoundException =>
            println (e.getMessage)
        }

      // Complain otherwise
      case _ =>
        println ("usage: run file.hip")
    }
  }
}


