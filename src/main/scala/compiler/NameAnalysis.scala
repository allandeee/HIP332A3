/**
  * Hipster cellular automaton language.
  * Name analyser for the Hipster language.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama._

/**
  * Attribute definitions for Hipster name analysis.
  */
trait NameAnalysis extends AttributionUtils {

  self : TypeAnalysis =>

  import HipsterTree._
  import SymbolTable._
  import ConstEval._
  import util.{Entity, MultipleEntity, UnknownEntity}

  def tree : HipsterTree

  /**
    * The declared dimension of the automaton, which we propagate
    * throughout the tree.
    */
  val dim : HipsterNode => Option[Int] =
    attr {
      case DimDecl(v) => Some(v.length)
      case tree.prev(l : TopLevelDecl) => dim(l)
      case tree.parent(p) => dim(p)
      case _ => None
    }

  /**
    * The environment containing all bindings visible at a particular
    * node in the tree, not including any that are defined at that node.
    */
  val envin : HipsterNode => Environment =
    attr {

      // If we are at the program node (root of tree) then the
      // environment in is a root environment prepopulated by
      // type signatures for built-in prelude functions.
      case p : HipsterProgram =>
        rootenv(
          ("fabs", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("abs", BuiltIn(IntType(), Vector(IntType()), false)),
          ("sin", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("cos", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("tan", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("asin", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("acos", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("atan", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("exp", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("log", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("log10", BuiltIn(FloatType(), Vector(FloatType()), false)),
          ("round", BuiltIn(IntType(), Vector(FloatType()), false)),
          ("floor", BuiltIn(IntType(), Vector(FloatType()), false)),
          ("ceil", BuiltIn(IntType(), Vector(FloatType()), false)),
          ("rnd", BuiltIn(IntType(), Vector(IntType()), true)),
          ("frnd", BuiltIn(FloatType(), Vector(), true)))

      // Plumbing for constructs that open a new scope.

      // Function declarations. The environment in of the first child
      // after the identifier child is the the environment in of the
      // parent with an extra scope added.
      case tree.parent.pair(n, p : FunctionDecl)
          if prevIsOfType[IdnDef](n) => enter(envin(p))

      // Initialiser declarations. The environment in of the first child
      // after the identifier child is the the environment in of the
      // parent with an extra scope added.
      case tree.parent.pair(n, p : InitialiserDecl)
          if prevIsOfType[IdnDef](n) => enter(envin(p))

      // Updater declarations. The environment in of the first child
      // is the environment in of the parent with an extra scope
      // added.
      case tree.parent.pair(n, p : UpdaterDecl)
          if tree.prev(n).isEmpty => enter(envin(p))

      // Mapper declarations. The environment in of the first child
      // is the environment in of the parent with an extra scope
      // added.
      case tree.parent.pair(n, p : ColourMapperDecl)
          if tree.prev(n).isEmpty => enter(envin(p))

      // Add a binding for the 'me' neighbour to the environment in of
      // the first neighbour definition of the neighbourhood declaration.
      case tree.parent.pair(n, p : NeighbourhoodDecl)
          if tree.prev(n).isEmpty =>
        defineIfNew(envin(p), "me",
          Neighbour(NeighbourVal(Vector().padTo(dim(p).getOrElse(0), 0))))

      // Block statement. The environment in of the first child
      // is the environment in of the parent with an extra scope
      // added.
      case tree.parent.pair(n, p : Block)
          if tree.prev(n).isEmpty => enter(envin(p))

      // The `from`, `to` and `step` expressions of a `for` loop are
      // evaluated with the environment in of the parent statement ...
      case tree.parent.pair(n : Expression, p : ForStmt)
          => envin(p)

      // ... and its body is evaluated with the environment in extended
      // with a binding for the control variable of the loop.
      case tree.parent.pair(n : Statement, ForStmt(i, _, _, _, _)) =>
        envout(i)

      // The neighbour set of an `iterate...over` statement is evaluated 
      // with the environment in its enclosing `iterate` statement....
      case tree.parent.pair(n : NeighbourSet, p : IterateOverStmt) =>
        envin(p)

      // ... but its body is evaluated with the environment in extended
      // with a binding for the control variable of the loop.
      case tree.parent.pair(n : Statement, IterateOverStmt(i, _, _)) =>
        envout(i)

      // Constant declarations. The initialiser expression
      // is evaluated with the environment in of the declaration...
      case tree.parent.pair(n : Expression, p : ConstantDecl) =>
        envin(p)

      // Variable declarations. The initialiser is evaluated
      // with the environment in of the declaration...
      case tree.parent.pair(n : Expression, p : VarDecl) =>
        p match {
          //... unless it is in a top-level state declaration
          // in which case it is evaluated with the environment
          // in of the enclosing state declaration.
          case tree.parent(pp : StateDecl) => envin(pp)
          case _ => envin(p)
        }

      // Default plumbing, if no other rules apply then a node
      // obtains its environment in from...

      // ... the environment out of its sibling to its immediate
      // left
      case tree.prev(l) =>
        envout(l)

      // ... and otherwise, the environment in of its parent.
      case tree.parent(p) =>
        envin(p)
    }

  /**
    * The environment containing all bindings visible "after" a
    * particular node in the tree.  I.e., it's the environment at the
    * node plus any new bindings introduced by the node.
    */
  val envout : HipsterNode => Environment =
    attr {

      // Function declarations. The environment out is equal to
      // the environment out of its defining identifier child.
      case FunctionDecl(i, _, _, _) =>
        envout(i)

      // Initialiser declarations. The environment out is equal to
      // the environment out of its defining identifier child.
      case InitialiserDecl(i,_) =>
        envout(i)

      // Updater declaration. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : UpdaterDecl =>
        envin(n)

      // Colour mapper declaration. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : ColourMapperDecl =>
        envin(n)

      // Block statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : Block =>
        envin(n)

      // `for` statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : ForStmt =>
        envin(n)

      // `if` statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its
      // `then` and `else` blocks.
      case n : IfStmt =>
        envin(n)

      // `cell` statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : CellStmt =>
        envin(n)

      // `iterate...over` statement. The environment out is equal to its
      // environment in, discarding any local bindings made in its body
      case n : IterateOverStmt =>
        envin(n)

      // Variable and constant declarations. The environment out is
      // equal to the environment out of its defining identifier child.
      case ConstantDecl(_, i, _) =>
        envout(i)

      case VarDecl(_, i, _) =>
        envout(i)

      // Default plumbing, if no other rules apply then a node
      // obtains its environment out from ...

      // ... the environment out of its right-most child ...
      case tree.lastChild(r) =>
        envout(r)

      // ... otherwise the environment out of a identifier definition
      // node is its environment in extended by a binding for that
      // identifier ...
      case n @ IdnDef(i) =>
        defineIfNew(envin(n), i, entity(n))

      // ... and if all else fails its environment out is equal to
      // its environment in.
      case n =>
        envin(n)
    }

  /**
    * The program entity referred to by an identifier definition or use.  In
    * the case of a definition it's the thing being defined, so define it to
    * be a reference to a new entity that represents that thing.  If it's
    * already defined, return an entity that indicates a multiple definition.
    * In the case of a use, it's the thing defined elsewhere that is being
    * referred to here, so look it up in the environment, using an unknown
    * entity if the environment does not already contain a binding.
    */
  val entity : IdnNode => Entity =
    attr {
      // At defining instances, if the identifier is already defined
      // in the current scope then attribute with the entity for
      // multiple definitions.
      case n @ IdnDef(i) if (isDefinedInScope(envin(n), i)) =>
        MultipleEntity()

      // At applied instances, if the identifier is defined in the
      // environment in at this node then return the corresponding
      // entity. Otherwise attribute with the unknown name entity.
      case n @ IdnUse (i) =>
        lookup(envin(n), i, UnknownEntity())

      // If the last case isn't triggered then we must be at an `IdnDef`
      // node, so use the enclosing context to work out the entity
      // associated with this defining instance:

      case tree.parent(p @ VarDecl(t, _, e)) =>
        p match {
          case tree.parent(_ : StateDecl) => StateField(t, e)
          case _ => Variable(t,e)
        }

      case tree.parent(ParamDecl(t,_)) =>
        Parameter(t)

      case tree.parent(ConstantDecl(t, _, e)) =>
        Constant(t,e)

      case tree.parent(NeighbourDecl(_, CoordExpr(v))) =>
        Neighbour(NeighbourVal(v.map(o =>
          value(o) match {
            case IntVal(i) => i
            case _ => 0
          })))

      case tree.parent(d @ FunctionDecl(_, p, r, s)) =>
        Function(
          r getOrElse(UnknownType()),
          p map {case ParamDecl(t,_) => t}, d)

      case tree.parent(InitialiserDecl(_,_)) =>
        Initialiser()

      case tree.parent(_ : IterateOverStmt) =>
        ControlVariable(NeighbourType())

      case tree.parent(_ : ForStmt) =>
        ControlVariable(IntType())

      case _ =>
        UnknownEntity()
    }
}
