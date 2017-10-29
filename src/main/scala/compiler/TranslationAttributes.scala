/**
  * Hipster cellular automaton language.
  * Attributes defined fo use in the Hipster to Java translator.
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
import HipsterTree.HipsterTree

trait TranslationAttributes extends AttributionUtils with Environments {

  self : NameAnalysis with TypeAnalysis =>

  import HipsterTree._
  import SymbolTable._
  import util.UnknownEntity
  import util.Comparison._
  
  def tree : HipsterTree

  // Irritatingly Java doesn't allow the shadowing of outer scope
  // variables by those declared in inner scopes whereas Hipster does.
  // This means that we must "uniquify" the names of variables in
  // inner scopes. The following attributes are used to achieve this goal.

  /**
    * An entity for representing translated names. These are 
    * uniquified and distinguished from names inherited from 
    * the superclass of the generated class using a `$` symbol.
    */
  case class TranslatedIdn(ident : String) extends NamedEntity

  /**
    * Add a new translation binding to an environment.
    */
  def addTranslation(env : Environment, n : String) : Environment =
    defineIfNew(env, n, TranslatedIdn(n ++ "$"))

  /**
    * Environment that binds names declared in a Hipster program
    * (and those introduced in the translator) to uniquified and
    * mangled versions.
    * 
    * Attribute giving incoming environment at each node. Follows
    * the same tree walk as the `envin` attribute of the name 
    * analyser.
    */
  val namesIn : HipsterNode => Environment =
    attr {
      case _ : HipsterProgram =>
        val e1 = rootenv()
        val e2 = addTranslation(e1, "_loc")
        val e3 = addTranslation(e2, "_other")
        val e4 = addTranslation(e3, "_theother")
        addTranslation(e4, "_off")
      case tree.parent.pair(n, p : FunctionDecl)
          if prevIsOfType[IdnDef](n) => enter(namesIn(p))
      case tree.parent.pair(n, p : InitialiserDecl)
          if prevIsOfType[IdnDef](n) => enter(namesIn(p))
      case tree.parent.pair(n, p : UpdaterDecl)
          if tree.prev(n).isEmpty => enter(namesIn(p))
      case tree.parent.pair(n, p : ColourMapperDecl)
          if tree.prev(n).isEmpty => enter(namesIn(p))
      case tree.parent.pair(n, p : NeighbourhoodDecl)
          if tree.prev(n).isEmpty => namesIn(p)
      case tree.parent.pair(n, p : Block)
          if tree.prev(n).isEmpty => enter(namesIn(p))
      case tree.parent.pair(n : Statement, ForStmt(i, _, _, _, _)) =>
        val e1 = enter(namesOut(i))
        val e2 = addTranslation(e1, "_dirn")
        val e3 = addTranslation(e2, "_end")
        addTranslation(e3, "_step")
      case tree.parent.pair(n : NeighbourSet, p : IterateOverStmt) =>
        namesIn(p)
      case tree.parent.pair(n : Statement, IterateOverStmt(i, _, _)) =>
        namesOut(i)
      case tree.parent.pair(n : Statement, p : CellStmt) =>
        val e1 = enter(namesIn(p))
        addTranslation(e1, "_cell")
      case tree.parent.pair(n : Expression, p : ConstantDecl) =>
        namesIn(p)
      case tree.parent.pair(n : Expression, p : VarDecl) =>
        p match {
          case tree.parent(pp : StateDecl) => namesIn(pp)
          case _ => namesIn(p)
        }
      case tree.prev(l) => namesOut(l)
      case tree.parent(p) => namesIn(p)
    }

  /**
    * Environment that binds names declared in a Hipster program
    * (and those introduced in the translator) to uniquified and
    * mangled versions.
    * 
    * Attribute giving outgoing environment at each node. Follows
    * the same tree walk as the `envout` attribute of the name 
    * analyser.
    */
  val namesOut : HipsterNode => Environment =
    attr {
      case FunctionDecl(i, _, _, _) => namesOut(i)
      case InitialiserDecl(i, _) => namesOut(i)
      case n : UpdaterDecl => namesIn(n)
      case n : ColourMapperDecl => namesIn(n)
      case n : Block => namesIn(n)
      case n : ForStmt => namesIn(n)
      case n : IfStmt => namesIn(n)
      case n : CellStmt => namesIn(n)
      case n : IterateOverStmt => namesIn(n)
      case ConstantDecl(_, i, _) => namesOut(i)
      case VarDecl(_, i, _) => namesOut(i)
      case tree.lastChild(r) => namesOut(r)
      case n @ IdnDef(i) => addTranslation(namesIn(n), i)
      case n => namesIn(n)
    }

  /**
    * Test to see if a particular node is a node of the 
    * source tree.
    */
  def isInTree(t : HipsterNode) : Boolean =
    same(t, tree.root) || (tree.parent.containsInDomain(t))

  /**
    * Get translated name of identifier defined in source tree
    */
  def getTargetName(n : IdnNode) : String =
    entity(n) match {
      // Handle the names of builtins as a special case,
      // because they are hard coded into the simulator.
      case _ : BuiltIn => "_" + n.idn
      // For everything else, look up the translated name
      // in the namesOut environment.
      case _ =>
        lookup(namesOut(n), n.idn, UnknownEntity()) match {
          case i : TranslatedIdn => i.id
          // Assuming our source tree has passed successfully through the
          // semantic analyser the following default case should never occur.
          case _ => "__error"
        }
    }

  /**
    * Get translated name of an identifier named `s`. The specific 
    * name returned depends on the `namesIn` attribute at the point in 
    * the source tree at which we are making a translation, as specified 
    * by the `n` parameter.
    */
  def getTargetName(n : HipsterNode, s : String) =
    lookup(namesIn(n), s, UnknownEntity()) match {
      case i : TranslatedIdn => i.id
      // Assuming our source tree has passed successfully through the
      // semantic analyser the following default case should never occur.
      case _ => "__error"
    }

}

