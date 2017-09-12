/**
  * Hipster cellular automaton language.
  * Semantic analyser for the Hipster language.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import HipsterTree.HipsterTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

/**
  * Attribute definitions of the Hipster semantic analyser.
  */
class SemanticAnalysis(val tree : HipsterTree) extends Attribution
    with NameAnalysis with TypeAnalysis {
  
  import HipsterTree._
  import SymbolTable._
  import ConstEval._
  import org.bitbucket.inkytonik.kiama.util.Messaging.{
    check, checkUse, collectMessages, Messages, message, noMessages}
  import org.bitbucket.inkytonik.kiama.util.{MultipleEntity, UnknownEntity}

  // Error reporting.

  /**
    * Collect the semantic error messages for a given tree.
    */
  lazy val errors : Messages =
    collectMessages(tree) {

      // Errors related to defining and applied instances of identifiers.

      case d @ IdnDef(i) if (entity(d) == MultipleEntity()) =>
        message (d, "'" + i + "' is declared more than once in current scope")

      case u @ IdnUse(i) if (entity(u) == UnknownEntity()) =>
            message (u, "'" + i + "' is not declared at this point")

      case u @ IdnUse(i) =>
        checkUse(entity(u)) {
          case _ : Applicable =>
            message(u, "function identifier '" + i +
              "' can only be used in a function call",
              !parentIsOfType[FunCallExpr](u))
          case _ : Initialiser =>
            message(u, "initialiser identifier '" + i +
              "' can't be used here")
        }

      // Type checking of expressions.
      case e : Expression =>
        check(e) {
          // Verify that function calls have the correct number of parameters.
          case f @ FunCallExpr(i,a) =>
            checkUse(entity(i)) {
              case _ : Initialiser => Vector()
              case v : Applicable =>
                message(f,
                  "malformed function call: argument count incorrect, " +
                    "expecting " + v.numParams + " found " + a.length,
                  v.numParams != a.length) ++
                message(f,
                  "function without return value called in expression",
                  !parentIsOfType[FunCallStmt](f) && v.tipe == UnknownType())
              case _  => message(f,
                "malformed function call: function name expected")} ++
            message(f, "colour mapper can't call this function" ,
              inMapper(f) && (
                mapperStateAccessFlag(f) ||
                  taintedByBuiltinFlag(f) ||
                  containsForOrCellFlag(f))) ++
            message(f, "updater can't call this function",
              inUpdater(f) && (
                updaterStateAccessFlag(f) ||
                  taintedByBuiltinFlag(f) ||
                  containsForOrCellFlag(f))) ++
            message(f, "initialiser can't call this function outside of " +
              "the body of a `cell` statement",
              inInitialiserOutsideCell(f) &&
                initialiserStateAccessFlag(f))

          case n : LValue =>
            check(n) {
              // Verify that neighbour state expressions 'nbr:state' have
              // operands of the right kind.
              case NeighbourExpr(i,s) =>
                checkUse(entity(s)) {
                  case _ : StateField => noMessages
                  case _ => message(s, "right hand operand of ':' must " +
                      "be a state field identifier")
                } 
            } ++
            message(n, "colour mapper can't write to the state of any cell",
              inMapper(n) && mapperStateAccessFlag(n) && lvalueFlag(n)) ++
            message(n, "colour mapper can't read state of neighbour cell",
              inMapper(n) && mapperStateAccessFlag(n) && !lvalueFlag(n)) ++
            message(n, "updater can't assign to state of neighbour cell",
              inUpdater(n) && updaterStateAccessFlag(n)) ++
            message(n, "initialiser can't read or write state outside of " +
              "the body of a `cell` statement",
              inInitialiserOutsideCell(n) && initialiserStateAccessFlag(n))
        } ++
        // Also type errors.
        message(e, "type error, expecting " +
          expTipe(e).map(s=>"'"+s+"'").mkString(" or ") +
          " found '" + tipe(e) + "'",
          tipe(e) != UnknownType() && !(expTipe(e) contains tipe(e)))

      // Verify that coordinate expressions...
      case c @ CoordExpr(o) =>
        // ...are preceeded by a dimension declaration.
        message(c, "dimension not declared", dim(c) == None) ++
        // ...have the right number of ordinates, as specified in the
        // dimension declaration.
        message(c, "incorrect coordinate dimension, expected " +
          dim(c).getOrElse(0) + " got " + o.length + ".",
          dim(c).getOrElse(o.length) != o.length)

      // Verify that identifiers in neighbour subset in `iterate...over`
      // statement are constants.
      case Subset(v) =>
        v.flatMap{case IdnExpr(i) =>
          message(i, "non-constant identifier in neighbour subset",
            entity(i) match {
              case _ : Constant => false
              case _ : Neighbour => false
              case _ : UnknownEntity => false
              case _ : MultipleEntity => false
              case _ => true
            })}

      // Errors related to statements.

      case s : Statement =>
        check(s) {
          // Check that lvalues in assignment statements may be assigned to.
          case AssignStmt(l, _) =>
            check(l) {
              case IdnExpr(i) =>
                checkUse(entity(i)) {
                  case _ : Constant | _ : ControlVariable | _ : Neighbour =>
                    message(i,
                      "can't assign to a constant or control variable")}}
          // Check that return statements inside functions with non-void
          // return types possess a return expression.
          case n @ ReturnStmt(None) =>
            message(n, "missing return value", returnTipe(n) != UnknownType())
          // Check that `for` statements don't occur in `updater` or
          // `mapper` bodies.
          case n : ForStmt =>
            message(n, "updater body can't contain a `for` statement",
              inUpdater(n)) ++
            message(n, "colour mapper body can't contain a `for` statement",
              inMapper(n))
          // Check that `cell` statements don't occur the body of an
          // updater or colour mapper.
          case n : CellStmt =>
            message(n, "updater body can't contain a `cell` statement",
              inUpdater(n)) ++
            message(n, "colour mapper body can't contain a `cell` statement",
              inMapper(n))
          case n : VarDecl =>
            message(n, "variable declarations must be enclosed in a block " +
              "in the 'then' or 'else' clause of an 'if' statement",
              parentIsOfType[IfStmt](n)) ++
            message(n, "variable declarations must be enclosed in a block " +
              "in the body of a 'cell' statement",
              parentIsOfType[CellStmt](n)) ++
            message(n, "variable declarations must be enclosed in a block " +
              "in the body of an 'iterate...over' statement",
              parentIsOfType[IterateOverStmt](n)) ++
            message(n, "variable declarations must be enclosed in a block " +
              "in the body of a 'for' statement",
              parentIsOfType[ForStmt](n))
        } ++
        message(s, "inaccessible statement (dead code)", inaccessible(s))

      // Top level declaration related errors.

      // Check that the coodinate expressions in a neighbourhood
      // declaration have constant ordinates.
      case NeighbourDecl(_, CoordExpr(v)) =>
        v.flatMap(e => message(e,
          "invalid ordinate in neighbour definition", !isConst(e)))

      // Verify that every control path in the body of a function
      // with a return value contains a `return` statement.
      case d @ FunctionDecl(_, _, Some(_), b) =>
        message(d, "missing `return` in body of function declaration",
          !b.exists(returnFlag))

      // Verify that every control path in the body of the colour
      // mapper contains a `return` statement.
      case d @ ColourMapperDecl(b) =>
        message(d, "missing `return` in body of colour mapper declaration",
          !b.exists(returnFlag))

      // Verify that a program has...
      case HipsterProgram(d) =>
        // ...exactly one dimension declaration
        (d.collect({case s : DimDecl => s}) match {
          case Vector() => message(d, "missing 'dimension' declaration")
          case _ +: s => s.flatMap(t =>
            message(t, "repeated 'dimension' declaration"))}) ++ 
        // ...exactly one neighbourhood declaration
        (d.collect({case s : NeighbourhoodDecl => s}) match {
          case Vector() => message(d, "missing 'neighbourhood' declaration")
          case _ +: s => s.flatMap(t =>
            message(t, "repeated 'neighbourhood' declaration"))}) ++
        // ...exactly one state declaration
        (d.collect({case s : StateDecl => s}) match {
          case Vector() => message(d, "missing 'state' declaration")
          case _ +: s => s.flatMap(t =>
            message(t, "repeated 'state' declaration"))}) ++
        // ...exactly one updater declaration
        (d.collect({case s : UpdaterDecl => s}) match {
          case Vector() => message(d, "missing 'updater' declaration")
          case _ +: s => s.flatMap(t =>
            message(t, "repeated 'updater' declaration"))}) ++
        // ...exactly one colour mapper declaration
        (d.collect({case s : ColourMapperDecl => s}) match {
          case Vector() => message(d, "missing 'mapper' declaration")
          case _ +: s => s.flatMap(t =>
            message(t, "repeated 'mapper' declaration"))})
    }

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
    * Flag to mark those values that appear on the left hand side of
    * an assignment statement.
    */
  val lvalueFlag : LValue => Boolean =
    attr {
      case tree.parent.pair(n, AssignStmt(l, _)) if n eq l => true
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
  val containsForOrCellFlag : HipsterNode => Boolean =
    attr {
      // Mark all calls to...
      case FunCallExpr(i, args) =>
        (entity(i) match {
          //... a function whose declarations is marked.
          case Function(_, _, d) => containsForOrCellFlag(d)
          case _ => false
        }) || args.exists(containsForOrCellFlag)

      // Mark `for` and `cell` statements.
      case _ : ForStmt => true
      case _ : CellStmt => true

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(containsForOrCellFlag) 
    }

  /**
    * Flag to mark those nodes that
    *   * access the state field of a neighbour cell, or
    *   * make an assignment to the state of any cell.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val mapperStateAccessFlag : HipsterNode => Boolean =
    attr {
      // Mark all neighbour expressions on left of assignment.
      case n : NeighbourExpr if lvalueFlag(n) => true

      // Mark all non-me neighbour expressions
      case NeighbourExpr(IdnExpr(i),_) =>
        entity(i) match {
          case Neighbour(c) => !c.isMe
          case Constant(_, e) =>
            value(e) match {
              case c : NeighbourVal => !c.isMe
              case _ => false
            }
          case Variable(NeighbourType(), _) => true
          case ControlVariable(NeighbourType()) => true
          case StateField(NeighbourType(), _) => true
          case Parameter(NeighbourType()) => true
          case _ => false
        }

      // Mark all identifier expressions on the left of assignment
      // statements whose identifier is declared to be a state field.
      case n @ IdnExpr(i) if lvalueFlag(n) =>
        entity(i) match {
          case _ : StateField => true
          case _ => false
        }

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
    * Flag to mark those nodes that make an assignment to the state of
    * a neighbour cell.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val updaterStateAccessFlag : HipsterNode => Boolean =
    attr {
      // Mark all non-me neighbour expressions on left of assignment.
      case n @ NeighbourExpr(IdnExpr(i), _) if lvalueFlag(n) =>
        entity(i) match {
          case Neighbour(c) => !c.isMe
          case Constant(_, e) =>
            value(e) match {
              case c : NeighbourVal => !c.isMe
              case _ => false
            }
          case Variable(NeighbourType(), _) => true
          case ControlVariable(NeighbourType()) => true
          case StateField(NeighbourType(), _) => true
          case Parameter(NeighbourType()) => true
          case _ => false
        }

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
    * Flag to mark those nodes that read or write the state of a cell
    * outside of the scope of the body of a `cell` statement.
    * 
    * Is "heredetary" in the sense that a node of another kind is marked
    * if one of its children is or if it is a call to a function whose 
    * declaration is marked.
    */
  val initialiserStateAccessFlag : HipsterNode => Boolean =
    attr {
      // Mark all neighbour expressions.
      case n : NeighbourExpr => true

      // Mark all identifier expressions whose identifier is declared
      // to be a state field.
      case IdnExpr(i) =>
        entity(i) match {
          case _ : StateField => true
          case _ => false
        }

      // Mark all calls to functions whose declarations are marked.
      case FunCallExpr(i, args) =>
        (entity(i) match {
          case Function(_, _, e) => initialiserStateAccessFlag(e)
          case _ => false
        }) || args.exists(initialiserStateAccessFlag)

      // Mark a `cell` statement if one of its ordinates is marked.
      // The marking of the body of a cell statement plays no part in
      // determining the marking of that statement.
      case CellStmt(CoordExpr(c), _) =>
        c.exists(initialiserStateAccessFlag)

      // ... otherwise mark any node which has a child that is marked.
      // In particular, nodes without children will not be marked.
      case n => tree.child(n).exists(initialiserStateAccessFlag)
    }

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
    * Flag to mark nodes in an initaliser declaration that are not
    * enclosed in the body of a `cell` statement.
    */
  val inInitialiserOutsideCell : HipsterNode => Boolean =
    attr {
      case tree.parent(_ : InitialiserDecl) => true
      case tree.parent.pair(n, CellStmt(_,s)) if n eq s => false
      case tree.parent(p) => inInitialiserOutsideCell(p)
      case _ => false
    }
}

