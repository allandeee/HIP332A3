/**
  * Hipster cellular automaton language.
  * Some useful extensions to the Kiama parsers library.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package org.bitbucket.dominicverity.hipster
package compiler

import org.bitbucket.inkytonik.kiama.parsing._

trait ParserExtras extends Parsers {
  self : ParsersBase =>

  def wrapWithError[T, U](
    p : => Parser[T],
    f : T => Either[U, String]) : Parser[U] = Parser {
    in =>
    p(in) match {
      case Success(t, out) =>
        f(t) match {
          case Left(u) =>
            Success(u, out)
          case Right(msg) =>
            Error(msg, in)
        }
      case result : NoSuccess =>
        result
    }
  }

  import HipsterTree._
  import Utils._

  val expression : PackratParser[Expression]

  lazy val lvalue : PackratParser[LValue] =
    nocut(
      idnUseE ~ (":" ~> idnUseE) ^^ NeighbourExpr |
        idnUseE ^^ IdnExpr | failure("lvalue expected"))

  lazy val leaf : PackratParser[Expression] =
    nocut(
      idnUseE ~ (":" ~> idnUseE) ^^ NeighbourExpr |
        idnUseE ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ FunCallExpr |
        idnUseE ^^ IdnExpr | floatExprE | intExprE |
        failure("identifier or numerical constant expected"))

  private lazy val intExprE : PackratParser[IntExpr] =
    wrapWithError(regex(intRegex) |
      failure("integer constant expected"), parseToInt) ^^ IntExpr

  lazy val intExpr : PackratParser[IntExpr] = nocut(intExprE)

  private lazy val floatExprE : PackratParser[FloatExpr] =
    wrapWithError(regex(floatRegex) |
      failure("floating point constant expected"), parseToDouble) ^^ FloatExpr

  lazy val floatExpr : PackratParser[FloatExpr] = nocut(floatExprE)

  lazy val reservedWords : List[String] = List(
    "dimension", "neighbourhood", "state", "updater", "initialiser",
    "mapper", "function", "int", "boolen", "float", "neighbour", "if",
    "then", "else", "iterate", "over", "all", "others", "for", "to",
    "step", "return", "case", "of", "otherwise", "true", "false")

  lazy val reservedWordsRegex =
    (reservedWords mkString ("","|","")).r 

  private lazy val ident : PackratParser[String] =
    wrapWithError(regex("[a-zA-Z][a-zA-Z0-9_]*".r) |
      failure("identifier expected"),
      ((s : String) =>
        s match {
          case reservedWordsRegex(_*) =>
            Right("reserved word found where identifier expected")
          case s => Left(s)}))

  private lazy val idnUseE : PackratParser[IdnUse] = ident ^^ IdnUse

  lazy val idnUse : PackratParser[IdnUse] = nocut(ident ^^ IdnUse)

  lazy val idnDef : PackratParser[IdnDef] = nocut(ident ^^ IdnDef)

  override val whitespace : Parser[String] =
    """(\s|(//.*\n))*""".r
}

