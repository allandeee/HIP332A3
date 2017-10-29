/**
  * Hipster cellular automaton language.
  * Miscellaneous semantic analysis arrtibutes for the Hipster language.
  * Mostly analyse code structure, performing simple dead code analysis etc.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

/**
  * Attribute definitions for Hipster name analysis.
  */
trait StructuralAnalysis extends AttributionUtils {

  self : NameAnalysis with TypeAnalysis =>

  import HipsterTree._
  import SymbolTable._

  /**
    * Flag those statements that are guaranteed to cause a 
    * return from the enclosing function.
    */
  val returnFlag : Statement => Boolean =
    attr {
      case ReturnStmt(_) => true

      case IfStmt(_, t, Some(e)) => returnFlag(t) && returnFlag(e)

      case IterateOverStmt(_, _, b) => returnFlag(b)
      case CellStmt(_, b) => returnFlag(b)

      case Block(b) => b.exists(returnFlag)

      case _ => false
    }

  /**
    * Flag those statements that are inaccessible because they
    * are immediately preceeded, in the current code block, by 
    * a statement that will always cause the current function to 
    * return.
    */
  val inaccessible : Statement => Boolean =
    attr {
      case tree.prev.pair(n : Statement, l : Statement)
          if parentIsOfType[Block](n) || parentIsOfType[TopLevelDecl](n) =>
        returnFlag(l)
      case _ => false
    }

  /**
    * Attribute to mark expressions on left hand side of assignments.
    */
  val isOnTheLeft : HipsterNode => Boolean =
    attr {
      case tree.parent.pair(n, AssignStmt(l,_)) if n eq l => true
      case tree.parent(p) => isOnTheLeft(p)
      case _ => false
    }

  /**
    * Attribute to mark those nodes of the source program that lie
    * within the body of a `cell` statement.
    */
  val isInCellBody : HipsterNode => Boolean =
    attr {
      case tree.parent.pair(n, CellStmt(_, s)) if n eq s => true
      case tree.parent(p) => isInCellBody(p)
      case _ => false
    }

  /**
    * Flag to mark those constructs whose bodies contain a `for` or `cell` 
    * statement.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val containsForOrCell : HipsterNode => Boolean =
    attr {
      // Mark `for` and `cell` statements.
      case _ : ForStmt => true
      case _ : CellStmt => true

      // Mark all calls to functions whose declarations are marked.
      case FunCallExpr(i, args) =>
        (entity(i) match {
          case Function(_, _, e) => containsForOrCell(e)
          case _ => false
        })

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(containsForOrCell) 
    }

  /**
    * Flag to mark those nodes that
    *   * contain a state field expression, or
    *   * make an assignment to the state of any cell.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val mapperStateAccessFlag : HipsterNode => Boolean =
    attr {
      // Mark all neighbour expressions.
      case n : NeighbourExpr => true

      // Mark all identifier expressions on the left of assignment
      // statements whose identifier is declared to be a state field.
      case n @ IdnExpr(i)
          if (isOnTheLeft(n) && entity(i).isInstanceOf[StateField]) => true

      // Mark all calls to functions whose declarations are marked.
      case FunCallExpr(i, args) =>
        (entity(i) match {
          case Function(_, _, e) => mapperStateAccessFlag(e)
          case _ => false
        }) || args.exists(mapperStateAccessFlag)

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(mapperStateAccessFlag)
    }

  /**
    * Flag to mark those nodes that make an assignment to a neighbour 
    * expression.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val updaterStateAccessFlag : HipsterNode => Boolean =
    attr {
      // Mark all neighbour expressions on left of assignment.
      case n : NeighbourExpr => isOnTheLeft(n)

      // Mark all calls to functions whose declarations are marked.
      case FunCallExpr(i, args) =>
        (entity(i) match {
          case Function(_, _, e) => updaterStateAccessFlag(e)
          case _ => false
        }) || args.exists(updaterStateAccessFlag) 

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(updaterStateAccessFlag)
    }

  /**
    * Flag to mark those constructs whose bodies contain references
    * to neighbour constants or cell state fields. Doesn't look inside 
    * the bodies of `cell` statements.
    * 
    * We say that identifiers of these kinds occuring in the body of
    * a `cell` statement are "guarded" by that statement.
    */
  val containsUnguardedNbrOrStateIdn : HipsterNode => Boolean =
    attr {
      // Mark all neighbour expressions.
      case n : NeighbourExpr => true

      // Mark all identifier expressions whose identifier is declared
      // to be a state field or a neighbour constant.
      case IdnExpr(i) =>
        entity(i) match {
          case _ : StateField | _ : Neighbour => true
          case _ => false
        }

      // Mark a `cell` statement if one of its ordinates is marked.
      // The marking of the body of a cell statement plays no part in
      // determining the marking of that statement.
      case CellStmt(CoordExpr(c), _) =>
        c.exists(containsUnguardedNbrOrStateIdn)

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(containsUnguardedNbrOrStateIdn)
    }

  // Classify functions into three groups:
  //
  //   1) Fully general - those that:
  //              - don't contain `cell` or `for` statements and
  //              - don't contain references to neighbour symbols and
  //              - only access state fields in field expressions.
  //
  //   2) Cell funtions - those that:
  //              - don't contain `cell` or `for` statements and
  //              - contain references to neighbour symbols or state
  //                fields outside of neighbour expressions.
  //
  //      so (1) + (2) = all functions whose bodies don't contain
  //                     `cell` or `for` statements. (disjoint union)
  //
  //   3) Initialiser functions - those that:
  //              - contain `cell` or `for` statements and
  //              - don't contain neighbour symbols outside of the
  //                body of a `cell` statement and
  //              - only access state fields in field expressions or
  //                within the body of a `cell` statement.
  //
  //   4) Illegal functions - everything else.
  //
  // Semantic rules:
  //   -- functions in class (1) can only call those in class (1)
  //   -- functions in class (2) can only call those in class (1) and (2)
  //   -- functions in class (3) can only call those in class (1) and (3)
  //   -- illegal functions in class (4) are not-allowed
  //   -- initialisers must be members of class (1) or (3)
  //   -- updater and mapper functions must be members of class (1) or (2)
  //
  // Functions in classes (1) and (3) compile to outer class methods
  // Functions in class (2) compile to inner class methods

  /**
    * Mark calls to fully general functions.
    */
  def isGeneralFunction (n : IdnUse) : Boolean =
    entity(n) match {
      case Function(_, _, d) => insideGeneralFunction(d)
      case _ => false
    }

  /**
    * Mark constructs in the bodies of fully general functions.
    */
  val insideGeneralFunction : HipsterNode => Boolean =
    attr {
      case n : FunctionDecl =>
        (! containsForOrCell(n)) && (! containsUnguardedNbrOrStateIdn(n))
      case tree.parent(p) => insideGeneralFunction(p)
      case _ => false
    }

  /**
    * Mark definitions of cell functions.
    */
  def isCellFunction (n : IdnUse) : Boolean =
    entity(n) match {
      case Function(_, _, d) => insideCellFunction(d)
      case _ => false
    }

  /**
    * Mark constructs in the bodies of cell functions.
    */
  def insideCellFunction : HipsterNode => Boolean =
    attr {
      case n : FunctionDecl =>
        (! containsForOrCell(n)) && containsUnguardedNbrOrStateIdn(n)
      case tree.parent(p) => insideCellFunction(p)
      case _ => false
    }

  /**
    * Mark definitions of initialiser functions.
    */
  def isInitialiserFunction (n : IdnNode) : Boolean =
    entity(n) match {
      case Function(_, _, d) => insideInitialiserFunction(d)
      case _ => false
    }

  /**
    * Mark constructs in the bodies of initialiser functions.
    */
  def insideInitialiserFunction : HipsterNode => Boolean =
    attr {
      case n : FunctionDecl => 
        containsForOrCell(n) && (! containsUnguardedNbrOrStateIdn(n))
      case tree.parent(p) => insideInitialiserFunction(p)
      case _ => false
    }

  /**
    * Mark definitions of illegal functions
    */
  def isIllegalFunction (n : FunctionDecl) : Boolean =
    containsForOrCell(n) && containsUnguardedNbrOrStateIdn(n)

  /**
    * Flag to mark those nodes that call a tainted built-in.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val taintedByBuiltinFlag : HipsterNode => Boolean =
    attr {
      // Mark all calls to...
      case FunCallExpr(i, args) =>
        (entity(i) match {
          //... functions whose declarations are marked and ...
          case Function(_, _, e) => taintedByBuiltinFlag(e)
          //... built-ins that are marked as tainted.
          case BuiltIn(_, _, b) => b
          case _ => false
        }) || args.exists(taintedByBuiltinFlag)

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(taintedByBuiltinFlag)
    }

  /**
    * Flag to mark nodes in an updater declaration.
    */
  val inUpdater : HipsterNode => Boolean =
    attr {
      case tree.parent(p : UpdaterDecl) => true
      case tree.parent(p) => inUpdater(p)
      case _ => false
    }

  /**
    * Flag to mark nodes in a colour mapper declaration.
    */
  val inMapper : HipsterNode => Boolean =
    attr {
      case tree.parent(p : ColourMapperDecl) => true
      case tree.parent(p) => inMapper(p)
      case _ => false
    }

  /**
    * Flag to mark nodes in an initaliser declaration.
    */
  val inInitialiser : HipsterNode => Boolean =
    attr {
      case tree.parent(_ : InitialiserDecl) => true
      case tree.parent.pair(n, CellStmt(_,s)) if n eq s => false
      case tree.parent(p) => inInitialiser(p)
      case _ => false
    }
}
