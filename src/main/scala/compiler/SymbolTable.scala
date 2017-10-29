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

import org.bitbucket.inkytonik.kiama._

import util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

  import HipsterTree._
  import ConstEval._
  import util.Entity

  // Entities describing defined identifiers.

  /**
    * Common base class of entities that contain a type.
    */
  abstract class TypedEntity extends Entity {
    def tipe : Type
  }

  /**
    * A variable entity. The optional expression component is the 
    * expression used to initialise the variable.
    */
  case class Variable(tipe : Type, exp : Option[Expression])
      extends TypedEntity

  /**
    * A control variable entity.
    */
  case class ControlVariable(tipe : Type)
      extends TypedEntity

  /**
    * A state field entity. The optional expression component specifies
    * the default value that this field of each cell is initialised to.
    */
  case class StateField(tipe : Type, exp : Option[Expression])
      extends TypedEntity

  /**
    * A parameter entity.
    */
  case class Parameter(tipe : Type) extends TypedEntity

  /**
    * A constant entity.  The expression component is the expression
    * that was used to define the constant.
    */
  case class Constant(tipe : Type, exp : Expression) extends TypedEntity

  /**
    * Common base class for entities that represent information
    * about functions (user defined or builtin) 
    */
  abstract sealed class Applicable extends TypedEntity {
    def paramTipes : Vector[Type]
    def numParams = paramTipes.length
  }

  /**
    * A function entity. Encapsulates the declared type signature
    * of a function, which comprises a list of parameter types and a 
    * return type. Also contains a reference to the corresponding top
    * level declaration.
    */
  case class Function(
    tipe : Type,
    paramTipes : Vector[Type],
    decl : FunctionDecl
  ) extends Applicable

  /**
    * A builtin entity. Encapsulates the type information associated
    * with a function built in to the Hipster "prelude".
    */
  case class BuiltIn(
    tipe : Type,
    paramTipes : Vector[Type],
    tainted : Boolean
  ) extends Applicable

  /**
    * A neighbour entity. The coordinate expression gives the coordinate
    * of the given neighbour relative to the current cell. 
    */
  case class Neighbour(coord : NeighbourVal) extends TypedEntity {
    val tipe = NeighbourType()
  }

  /**
    * An initialiser function entity.
    */
  case class Initialiser() extends Entity

}
