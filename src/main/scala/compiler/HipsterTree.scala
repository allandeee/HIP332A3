/**
  * Hipster cellular automaton language.
  * Abstract syntax tree type declarations. 
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package org.bitbucket.dominicverity.hipster
package compiler

/**
 * Module containing structures for representing Hipster language programs.
 */
object HipsterTree {

  /**
    * Interface for all Hipster tree nodes.
    */
  sealed abstract class HipsterNode extends Product

  /**
    * A Hipster program comprises a sequence of top level declarations.
    */
  case class HipsterProgram (stmts : Vector[TopLevelDecl]) extends HipsterNode

  /**
    * Abstract base class for all top level declarations.
    */
  sealed abstract class TopLevelDecl extends HipsterNode

  /**
    * Declaration for automaton dimension. 
    * At the moment we only support 1D and 2D automata
    * Each dimension is accompanied by a flag which specifies
    * whether it wraps around at its boundary.
    */
  case class DimDecl(dims : Vector[(Expression, Boolean)])
      extends TopLevelDecl

  /**
    * Neighbourhood declaration.
    * Each of the cells to which the current cell has direct access
    * is listed in this declaration.
    */
  case class NeighbourhoodDecl(nbrs : Vector[(IdnDef, CoordExpr)])
      extends TopLevelDecl

  /**
    * State declaration. Speficies the data fields possessed by each cell.
    */
  case class StateDecl(flds : Vector[VarDecl]) extends TopLevelDecl

  /**
    * Top level constant declaration.
    */
  case class ConstantDecl(decl : VarDecl) extends TopLevelDecl

  /**
    * Updater declaration. Contains the code which will be executed to
    * update the state of each cell. Like the Highlander, there can only
    * be one of these.
    */
  case class UpdaterDecl(stmts : Vector[Statement]) extends TopLevelDecl

  /**
    * Initialiser declaration. Contains code to set the initial states
    * of the cells of the automaton. There can be a number of these to 
    * initialise things in different ways, and the particular initiliser
    * executed at startup can be selected in the animator GUI.
    */
  case class InitialiserDecl(idn : IdnDef, stmts : Vector[Statement])
      extends TopLevelDecl

  /**
    * Colour mapper declaration. This is simply a function that returns 
    * a colour computed from the state of the current cell. Colours are 
    * represented by sRGB integers comprising three 8-bit fields giving 
    * blue (bits 0-7), green (bits 8-15) and red (bits 16-23) colour
    * channel intensities. Utility functions and hexadecimal constants
    * are provided to allow these colour values to be manipulated.
    */
  case class ColourMapperDecl(stmts : Vector[Statement])
      extends TopLevelDecl

  /**
    * Function declaration. All functions are declared at top level.
    */
  case class FunctionDecl(
    idn : IdnDef,
    params : Vector[(Type, IdnDef)],
    ret : Option[Type],
    stmts : Vector[Statement]) extends TopLevelDecl

  /**
    * Abstract class for declaration types.
    */
  sealed abstract class Type extends HipsterNode

  /**
    * Classes to represent the declared types of variables.
    */
  case class IntType() extends Type
  case class BoolType() extends Type
  case class FloatType() extends Type
  case class NeighbourType() extends Type
 
  // Identifiers names are represented in raw string form.
  type Identifier = String

  /**
    * Abstract class for identifier references.
    */
  abstract class IdnNode extends HipsterNode {
    def idn : String
  }

  /**
    * A defining occurrence of an identifier.
    */
  case class IdnDef(idn : Identifier) extends IdnNode

  /**
    * An applied occurence (use) of an identifier.
    */
  case class IdnUse (idn : Identifier) extends IdnNode

  /**
    * Variable declaration. 
    */
  case class VarDecl (
    tipe : Type,
    idn : IdnDef,
    init : Option[Expression]) extends Statement

  /**
    * Superclass of all statements
    */
  sealed abstract class Statement extends HipsterNode

  /**
    * A block comprising a (possibly empty) list of statements.
    */
  case class Block(stmts : Vector[Statement]) extends Statement

  /**
    * Assignment statement.
    */
  case class AssignStmt(lval : LValue, exp : Expression) extends Statement

  /**
    * An <if> statement.
    */
  case class IfStmt(
    exp : Expression,
    thn : Statement,
    els : Option[Statement]) extends Statement

  /**
    * <iterate> statement for iterating over sets of neighbours.
    */
  case class IterateOverStmt(
    idn : IdnDef,
    nbrs : NeighbourSet,
    body : Statement) extends Statement

  /**
    * Superclass of classes specifying sets of neighbours.
    */
  sealed abstract class NeighbourSet extends HipsterNode

  /**
    * Case classes for specifying sets of neighbours to iterate over.
    */
  case class All() extends NeighbourSet  // All including current cell.
  case class Others() extends NeighbourSet // All but current cell.
  case class Subset(nbrs : Vector[IdnUse]) extends NeighbourSet // Specified subset of neighbours.

  /**
    * A general <for> loop, iterating over integer ranges. Semantics:
    * - The <start>, <stop>, and <step> expressions are evaluated on 
    *   entry and are not then re-evaluated. 
    * - If <step> is 0 then we terminate without executing the body.
    * - If <start> = <stop> and <step> ≠ 0 then we execute the body once.
    */
  case class ForStmt(
    idn : IdnDef,
    start : Expression,
    end : Expression,
    step : Option[Expression],
    body : Statement) extends Statement

  /**
    * Function call statement.
    */
  case class FunCallStmt(idn : IdnUse, args : Vector[Expression])
      extends Statement

  /**
    * <return> statement.
    */
  case class ReturnStmt(v : Option[Expression]) extends Statement

  /**
    * <empty> statement.
    */
  case class EmptyStmt() extends Statement

  /**
    * The <cell> construct sets the current cell for use in the 
    * statement (or block of statements). 
    * 
    * To preserve the local nature of cell updating, this cannot 
    * be called from within the updater and colour mapper functions,
    * or fomr any function called by either of those.
    */
  case class CellStmt(
    coord : CoordExpr,
    body : Statement 
  ) extends Statement

  /**
    * Superclass of all expressions.
    */
  sealed abstract class Expression extends HipsterNode

  /** 
    * Integer value expression.
    */
  case class IntExpr(value : Int) extends Expression

  /**
    * Float value expression.
    */
  case class FloatExpr(value : Double) extends Expression

  /**
    * Superclass of the boolean constants.
    */
  sealed abstract class BoolExpr() extends Expression

  /**
    * Boolean TRUE expression.
    */
  case class TrueExpr() extends BoolExpr

  /**
    * Boolean FALSE expression.
    */
  case class FalseExpr() extends BoolExpr

  /**
    * Common interface for binary expressions.
    */
  abstract class BinaryExpression extends Expression {
    def left : Expression
    def right : Expression
  }

  /**
    * Common interface for unary expressions.
    */
  abstract class UnaryExpression extends Expression {
    def exp : Expression
  }

  /**
    * Addition expression.
    */
  case class PlusExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Subtraction expression.
    */
  case class MinusExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Multiplication expression.
    */
  case class MultExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Division expression.
    */
  case class DivExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Remainder (modulus) expression.
    */
  case class ModExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Negation expression.
    */
  case class NegExpr(exp : Expression) extends UnaryExpression

  /**
    * Boolean conjunction (AND) expression.
    */
  case class AndExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Boolean conjunction (OR) expression.
    */
  case class OrExpr(left : Expression, right : Expression)
      extends BinaryExpression
  
  /**
    * Boolean not expression.
    */
  case class NotExpr(exp : Expression) extends UnaryExpression

  /**
    * Equality relation expression.
    */
  case class EqualExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Less than relation expression.
    */
  case class LessExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Greater than relation expression.
    */
  case class GreaterExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Less than or equal relation expression.
    */
  case class LessEqExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Greater than or equal relation expression.
    */
  case class GreaterEqExpr(left : Expression, right : Expression)
      extends BinaryExpression

  /**
    * Function call expression.
    */
  case class FunCallExpr(idn : IdnUse, args : Vector[Expression])
      extends Expression

  /**
    * A coordinate expession - just a tuple of integers.
    */
  case class CoordExpr(ords : Vector[Expression]) extends Expression

  /**
    * Base class for all lvalue expressions
    */
  sealed abstract class LValue() extends Expression

  /**
    * Identitifier expression
    */
  case class IdnExpr(idn : IdnUse) extends LValue

  /**
    * Neighbour expression, <neighbour>:<field> 
    * Used to access a specified field in the state 
    * of a specified neighbour.
    * 
    * <neighbour> can either be a constant neighbour name
    * or a variable bound in an iterate loop.
    */
  case class NeighbourExpr(nbr : IdnUse, fld : IdnUse) extends LValue

}
