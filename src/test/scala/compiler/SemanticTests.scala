/**
  * Hipster cellular automaton language.
  * Semantic analysis test support
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * © 2013, Anthony Sloane, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.bitbucket.inkytonik.kiama.util.Messaging

/**
  * Support code for semantic test suites.
  */
class SemanticTests extends ParseTests with Messaging {

  import HipsterTree._
  import org.bitbucket.inkytonik.kiama.parsing.{Error, Failure, Success}
  import org.bitbucket.inkytonik.kiama.util.Source
  import org.bitbucket.inkytonik.kiama.util.StringSource
  import org.bitbucket.inkytonik.kiama.util.FileSource
  import org.bitbucket.inkytonik.kiama.util.Messaging.Messages

  val parsers = new SyntaxAnalysis (positions)

  /**
    * Parse test input from a source and, if the parse succeeds with no 
    * input left, return the program tree. If the parse fails, 
    * fail the test.
    */
  def parseProgram(src : Source) : HipsterProgram =
    parsers.parse(parsers.parser, src) match {
      case Success(r, in) =>
        if (!in.atEnd) fail ("input remaining at " + in.pos)
        r
      // Parsing failed, so report it
            case f : Error =>
                fail ("parse error: " + f.message)
            case f : Failure =>
                fail ("parse failure: " + f.message)
    }

  /**
    * Parse some test input and run the semantic analyser over the resulting
    * tree (if the parse succeeds).
    */
  def semanticTest(src : Source) : Messages = {
    val prog = parseProgram(src)
    val tree = new HipsterTree(prog)
    val analysis = new SemanticAnalysis(tree)
    assert(analysis.envout(prog).length > 0, "scope error, global scope missing")
    assert(analysis.envout(prog).length === 1, "scope error, unclosed local scope")
    val messages = analysis.errors.sorted
    // println (messages)
    messages
  }

  /**
    * Parse some test input from a string and run the semantic analyser.
    */
  def semanticTestInline(src : String) : Messages =
    semanticTest(StringSource(src))

  /**
    * Parse some test input from a string and run the semantic analyser.
    */
  def semanticTestFile(filename : String) : Messages =
    semanticTest(FileSource(filename))

  /**
    * Assert that a message was produced at a given position.
    */
  def assertMessage (
    messages : Messages, index : Int,
    line : Int, column : Int, msg : String) : Unit = {
    val m = messages(index)
    m.label shouldBe msg
    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }

  // Most of the tests of type attribution work by triggering a type error
  // and then using the reported types in the resulting message to verify
  // either an expected or inferred type.

  // The following assertion methods support this approach, by using
  // a regular expression to analyse a type error message and extract
  // the reported expected and inferred types.

  /**
    * Regexp to analyse type error messages.
    */
  val TypeErrorRegex =
    """^type error, expecting ('.+?'( or '.+?')*) found '(.+?)'$""".r
  val typeRegex = """'(.+?)'""".r

  /**
    * Assert that a message is a type error with specified list of
    * expected types.
    */
  def assertTypeErrorWithExpectedType (
    messages : Messages, index : Int,
    line : Int, column : Int, tipes : Set[String]) : Unit = {
    val m = messages(index)

    m.label match {
      case TypeErrorRegex(exp,_,_) =>
        val found =
          (for(m <- typeRegex findAllMatchIn exp) yield m.group(1)).toSet
        found shouldBe tipes
      case _ => fail("not a type error message")
    }

    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }

  /**
    * Assert that a message is a type error with specified 
    * inferred type.
    */
  def assertTypeErrorWithInferredType (
    messages : Messages, index : Int,
    line : Int, column : Int, tipe : String) : Unit = {
    val m = messages(index)

    m.label match {
      case TypeErrorRegex(_,_,inf) => inf shouldBe tipe
      case _ => fail("not a type error message")
    }

    positions.getStart (m.value) match {
      case Some (posn) =>
        posn.line shouldBe line
        posn.column shouldBe column
      case _ =>
        fail ("no position for message value")
    }
  }
}
