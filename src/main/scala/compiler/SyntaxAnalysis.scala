/**
  * Hipster cellular automaton language.
  * Parser for Hipster source code.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
  * Module containing parsers for the Hipster language.
  */
class SyntaxAnalysis (positions : Positions)
    extends Parsers (positions) with ParserExtras {

  import HipsterTree._
  import scala.language.postfixOps

  lazy val parser : PackratParser[HipsterProgram] =
    phrase (program)

  lazy val program : PackratParser[HipsterProgram] =
    (topLevelDecl+) ^^ HipsterProgram

  // Parsers for top level declarations.

  lazy val topLevelDecl : PackratParser[TopLevelDecl] =
    dimDecl | neighbourhoodDecl | stateDecl | constantDecl |
      updaterDecl | initialiserDecl | colourMapperDecl |
      functionDecl

  lazy val dimDecl : PackratParser[DimDecl] =
    keyword("dimension") ~>
      "(" ~> rep1sep(dimension, ",") <~ ")" <~ ";" ^^ DimDecl

  lazy val dimension : PackratParser[Dimension] =
    expression ~ opt(keyword("cyclic")) ^^ {
      case e ~ Some(_) => CyclicDim(e)
      case e ~ None => BoundedDim(e)
    }

  lazy val nbrDef : PackratParser[NeighbourDecl] =
    (idnDef <~ "=") ~ coordExpr ^^ NeighbourDecl

  lazy val neighbourhoodDecl : PackratParser[NeighbourhoodDecl] =
    keyword("neighbourhood") ~>rep1sep(nbrDef,",") <~ ";" ^^ NeighbourhoodDecl

  lazy val stateDecl : PackratParser[StateDecl] =
    keyword("state") ~> "{" ~> rep(varDecl) <~ "}" ^^ StateDecl

  lazy val constantDecl : PackratParser[ConstantDecl] =
    tipe ~ idnDef ~ ("=" ~> expression) <~ ";" ^^ ConstantDecl

  lazy val updaterDecl : PackratParser[UpdaterDecl] =
    keyword("updater") ~> "{" ~> rep(statement) <~ "}" ^^ UpdaterDecl

  lazy val initialiserDecl : PackratParser[InitialiserDecl] =
   keyword( "initialiser") ~> idnDef ~ ("{" ~> rep(statement) <~ "}") ^^
      InitialiserDecl

  lazy val colourMapperDecl : PackratParser[ColourMapperDecl] =
    keyword("mapper") ~> "{" ~> rep(statement) <~ "}" ^^ ColourMapperDecl

  lazy val paramDecl : PackratParser[ParamDecl] =
    tipe ~ idnDef ^^ ParamDecl

  lazy val functionDecl : PackratParser[FunctionDecl] =
    keyword("function") ~> idnDef ~ ("(" ~> repsep(paramDecl, ",") <~ ")") ~
      opt(":" ~> tipe) ~ ("{" ~> rep(statement) <~ "}") ^^ FunctionDecl

  // Parsers for variable declarations.

  lazy val varDecl : PackratParser[VarDecl] =
    tipe ~ idnDef ~ opt("=" ~> expression) <~ ";" ^^ VarDecl

  lazy val tipe : PackratParser[Type] =
    keyword("int") ^^^ IntType() |
      keyword("boolean") ^^^ BoolType() |
      keyword("float") ^^^ FloatType() |
      keyword("neighbour") ^^^ NeighbourType()

  // Parsers for statements

  lazy val statement : PackratParser[Statement] =
    varDecl | ifStmt | iterateOverStmt | forStmt | returnStmt |
      cellStmt | assignStmt | funCallStmt | block | emptyStmt

  lazy val block : PackratParser[Block] =
    "{" ~> rep(statement) <~ "}" ^^ Block

  lazy val emptyStmt : PackratParser[EmptyStmt] =
    ";" ^^^ EmptyStmt()

  lazy val assignStmt : PackratParser[AssignStmt] =
    (lvalue <~ "=") ~ (expression <~ ";") ^^ AssignStmt

  lazy val ifStmt : PackratParser[IfStmt] =
    (keyword("if") ~> expression) ~ (keyword("then") ~> statement) ~
      opt(keyword("else") ~> statement) ^^ IfStmt

  lazy val iterateOverStmt : PackratParser[IterateOverStmt] =
    (keyword("iterate") ~> idnDef) ~ (keyword("over") ~> neighbourSet) ~
      statement ^^ IterateOverStmt

  lazy val neighbourSet : PackratParser[NeighbourSet] =
    keyword("all") ^^^ All() |
      keyword("others") ^^^ Others() |
      "[" ~> rep1sep(idnExpr, ",") <~ "]" ^^ Subset

  lazy val forStmt : PackratParser[ForStmt] =
    (keyword("for") ~> idnDef) ~ ("=" ~> expression) ~ (keyword("to") ~>
      expression) ~ opt(keyword("step") ~> expression) ~ statement ^^ ForStmt

  lazy val funCallStmt : PackratParser[FunCallStmt] =
    funCallExpr <~ ";" ^^ FunCallStmt

  lazy val returnStmt : PackratParser[ReturnStmt] =
    keyword("return") ~> opt("(" ~> expression <~ ")") <~ ";" ^^ ReturnStmt

  lazy val cellStmt : PackratParser[CellStmt] =
    keyword("cell") ~> coordExpr ~ statement ^^ CellStmt

  // Expression parsers.
  // Precidence and associativity rules implemented in the
  // time honoured fashion.

  override lazy val expression : PackratParser[Expression] =
    expression ~ ("&&" ~> expression2) ^^ AndExpr |
      expression ~ ("||" ~> expression2) ^^ OrExpr |
      expression2

  lazy val expression2 : PackratParser[Expression] =
    expression1 ~ ("==" ~> expression1) ^^ EqualExpr |
      expression1 ~ ("<=" ~> expression1) ^^ LessEqExpr |
      expression1 ~ (">=" ~> expression1) ^^ GreaterEqExpr |
      expression1 ~ ("<" ~> expression1) ^^ LessExpr |
      expression1 ~ (">" ~> expression1) ^^ GreaterExpr |
      expression1

  lazy val expression1 : PackratParser[Expression] =
    expression1 ~ ("+" ~> expression0) ^^ PlusExpr |
      expression1 ~ ("-" ~> expression0) ^^ MinusExpr |
      expression0

  lazy val expression0 : PackratParser[Expression] =
    expression0 ~ ("*" ~> factor) ^^ MultExpr |
      expression0 ~ ("/" ~> factor) ^^ DivExpr |
      expression0 ~ ("%" ~> factor) ^^ ModExpr |
      factor

  lazy val factor : PackratParser[Expression] =
    "true" ^^^ TrueExpr() |
      "false" ^^^ FalseExpr() |
      "!" ~> factor ^^ NotExpr |
      "-" ~> factor ^^ NegExpr |
      "+" ~> factor |
      "(" ~> expression <~ ")" | leaf

  lazy val coordExpr : PackratParser[CoordExpr] =
    "[" ~> rep1sep(expression, ",") <~ "]" ^^ CoordExpr

}
