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

import org.bitbucket.inkytonik.kiama._

import HipsterTree.HipsterTree
import attribution.Attribution

/**
  * Attribute definitions of the Hipster semantic analyser.
  */
class SemanticAnalysis(val tree : HipsterTree) extends Attribution
    with NameAnalysis with TypeAnalysis with StructuralAnalysis {
  
  import HipsterTree._
  import SymbolTable._
  import util.Messaging.{check, checkUse, collectMessages, Messages, message}
  import util.{MultipleEntity, UnknownEntity}

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
            message(f, "colour mapper can't call a tainted function" ,
              inMapper(f) && taintedByBuiltinFlag(f)) ++
            message(f, "function called by colour mapper makes illegal " ++
              "state access", inMapper(f) && mapperStateAccessFlag(f)) ++
            message(f, "colour mapper can't call an initialiser function" ,
              inMapper(f) && isInitialiserFunction(i)) ++
            message(f, "updater can't call a tainted function",
              inUpdater(f) && taintedByBuiltinFlag(f)) ++
            message(f, "function called by updater makes illegal " ++
              "state access", inUpdater(f) && updaterStateAccessFlag(f)) ++
            message(f, "updater can't call an initialiser function",
              inUpdater(f) && isInitialiserFunction(i)) ++
            message(f, "initialiser can't call a cell function",
              inInitialiser(f) && isCellFunction(i)) ++
            message(f, "general function can't call a cell function",
              insideGeneralFunction(f) && isCellFunction(i)) ++
            message(f, "initialiser function can't call a cell function",
              insideInitialiserFunction(f) && isCellFunction(i)) ++
            message(f, "cell function can't call an initialiser function",
              insideCellFunction(f) && isInitialiserFunction(i))

          case n : LValue =>
            check(n) {
              // Verify that neighbour state expressions 'nbr:state' have
              // operands of the right kind.
              case NeighbourExpr(_,s)
                  if (!entity(s).isInstanceOf[StateField]) =>
                message(s, "right hand operand of ':' must be a state " +
                  "field identifier")
              case NeighbourExpr(_,s)
                  if (inInitialiser(n) && !isInCellBody(n)) =>
                message(s, "initialiser can't access state outside of the " +
                  "body of a `cell` statement")
              case IdnExpr(s)
                  if (inInitialiser(n) && !isInCellBody(n))=>
                message(n, "initialiser can't access state outside of the " +
                  "body of a `cell` statement",
                  entity(s).isInstanceOf[StateField]) ++
                message(n, "initialiser can't reference neighbour symbol " +
                  "outside of the body of a `cell` statement",
                  entity(s).isInstanceOf[Neighbour])
            } ++
            message(n, "colour mapper can't write to the state of any cell",
              inMapper(n) && mapperStateAccessFlag(n) && isOnTheLeft(n)) ++
            message(n, "colour mapper can't read state of neighbour cell",
              inMapper(n) && mapperStateAccessFlag(n) && !isOnTheLeft(n)) ++
            message(n, "updater can't assign to state of neighbour cell",
              inUpdater(n) && updaterStateAccessFlag(n))
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

      case d @ ConstantDecl(NeighbourType(), _, _) =>
        message(d, "can't alias a neighbour symbol in a constant declaration")

      // Check that the coodinate expressions in a neighbourhood
      // declaration have constant ordinates.
      case NeighbourDecl(_, CoordExpr(v)) =>
        v.flatMap(e => message(e,
          "invalid ordinate in neighbour definition", !isConst(e)))

      // Verify that every control path in the body of a function
      // with a return value contains a `return` statement.
      case d @ FunctionDecl(_, _, Some(_), b) =>
        message(d, "missing `return` in body of function declaration",
          !b.exists(returnFlag)) ++
        message(d, "illegal function, contains `for` or `cell` statement " ++
          "and an unguarded reference to a neighbour or state field.",
          isIllegalFunction(d))

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
}

