/**
  * This file is part of Kiama.
  * Adapted for the Hipster to Java translator.
  *
  * © 2014-2017 Anthony M Sloane, Macquarie University.
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  *
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama._
import output.ParenPrettyPrinter
import output.PrettyExpression
import output.PrettyPrinterTypes.Layout

/**
  * Abstract syntax tree pretty-printing for Minijava.
  */
class PrettyPrinter extends ParenPrettyPrinter {

  import MiniJavaTree._
  
  /**
    * Format a MiniJava node.
    */
  def format(t : MiniJavaNode) : Layout =
    layout(toDoc(t))

  /**
    * Convert a MiniJava AST node to a pretty-printing document.
    */
  def toDoc(t : MiniJavaNode) : Doc =
    link(t, toDocNoLink(t))

  def toDocNoLink(t : MiniJavaNode) : Doc =
    t match {
      case Class(ms, i, sc, mbrs) =>
        line <> modsToDoc(ms) <> "class" <+> toDoc(i) <>
        sc.map(n => space <> "extends" <+>
        toDoc(n)).getOrElse(emptyDoc) <+>
        braces(
          nest(
            line <> line <>
              vsep(mbrs map toDoc)
          ) <>
          line
        ) 
      case Field(ms, t, i, e) =>
        modsToDoc(ms) <> toDoc(t) <+> toDoc(i) <>
        e.map(n => space <> "=" <+> toDoc(n)).getOrElse(emptyDoc) <> semi
      case Method(ms, r, i, as, bs) =>
        line <> modsToDoc(ms) <> toDoc(r) <>
        toDoc(i) <> parens(nest(fillsep(as map toDoc, comma))) <+>
        braces(
          nest((
            r match {
              case ConstructWithSuper(es) => 
                line <>
                "super" <> parens(nest(fillsep(es map toDoc, comma))) <> semi
              case _ => emptyDoc
            }) <> (
            if (bs.isEmpty)
              emptyDoc
            else
              line <>
              vsep(bs map toDoc)
          )) <>
          line
        )
      case StaticInit(bs) =>
        line <> "static" <+>
        braces(
          nest(
            line <>
            vsep(bs map toDoc) 
          ) <>
          line
        )
      case Void() => "void" <> space
      case Construct() => emptyDoc
      case ConstructWithSuper(_) => emptyDoc
      case ReturnType(t) => toDoc(t) <> space
      case t : Type => t.toString
      case m : Modifier => m.toString
      case Argument(t, i) =>
        toDoc(t) <+> toDoc(i)
      case Varargs(t, i) =>
        toDoc(t) <> "..." <+> toDoc(i)
      case Return(e) =>
        "return" <> e.map(n => space <>
        toDoc(n)).getOrElse(emptyDoc) <> semi
      case Block(ss) =>
        braces(
          ss match {
            case Vector() => space
            case _ =>
              nest(
                line <>
                  vsep(ss map toDoc)
              ) <>
              line
          })
      case Var(t, i, e) =>
        toDoc(t) <+> toDoc(i) <>
        e.map(n => space <> "=" <+> toDoc(n)).getOrElse(emptyDoc) <> semi
      case If(e, s1, Empty()) =>
        "if" <+> parens(toDoc(e)) <> bodyToDoc(s1)
      case If(e, s1 , s2) =>
        "if" <+> parens(toDoc(e)) <>
        bodyToDoc(s1) <>
        line <>
        "else" <>
        bodyToDoc(s2)
      case While(e, s) =>
        "while" <+> parens(toDoc(e)) <> bodyToDoc(s)
      case Assign(l, e) =>
        toDoc(l) <+> equal <+> toDoc(e) <> semi
      case Call(b, i, as) =>
        b.map(n => toDoc(n) <> dot).getOrElse(emptyDoc) <>
        toDoc(i) <> parens(nest(fillsep(as map toDoc, comma))) <> semi
      case Empty() => emptyDoc
      case ArrayDims(ds) =>
        folddoc(ds.map(d => enclose("[", toDoc(d), "]")), _ <> _)
      case ConstructorArgs(as) =>
        parens(nest(fillsep(as map toDoc, comma)))
      case n : IdnTree => n.idn
      case e : PrettyExpression =>
        toParenDoc(e)
    }

  def modsToDoc(ms : Vector[Modifier]) : Doc =
    folddoc(ms.map(m => m.toString <> space), _ <> _)

  def bodyToDoc(s : Statement) : Doc =
    s match {
      case _ : Block => space <> toDoc(s)
      case _ => 
        nest(
          line <>
            toDoc(s)
        )
    }

  override def toParenDoc(e : PrettyExpression) : Doc =
    link(e, toParenDocNoLink(e))

  def toParenDocNoLink(e : PrettyExpression) : Doc =
    e match {
      case IndExp(b, e) =>
        toDoc(b) <> brackets(toDoc(e))
      case FieldExp(b, f) =>
        toDoc(b) <> dot <> toDoc(f)
      case CallExp(b, i, as) =>
        b.map(n => toDoc(n) <> dot).getOrElse(emptyDoc) <>
        toDoc(i) <> parens(hsep(as map toDoc, comma))
      case LambdaExp(as, b) =>
        parens(
          parens(nest(fillsep(as map toDoc, comma))) <+> "->" <+>
            (b match {
              case _ : Block => toDoc(b)
              case _ => braces(space <> toDoc(b) <> space)
            }))
      case CastExp(t, e) =>
        parens(toDoc(t)) <>
        (e match {
          case _ : IndExp | _ : FieldExp | _ : CallExp | _ : LambdaExp |
              _ : IdnExp | _ : ThisExp => toDoc(e)
          case _ => parens(toDoc(e))
        })
      case IntExp(v) =>
        value(v)
      case DoubleExp(v) =>
        value(v)
      case TrueExp() =>
        "true"
      case FalseExp() =>
        "false"
      case StringExp(s) =>
        dquotes(s)
      case IdnExp(i) =>
        toDoc(i)
      case ThisExp() =>
        "this"
      case NewExp(i,t) =>
        "new" <+> toDoc(i) <> toDoc(t)
      case _ =>
        super.toParenDoc(e)
    }
}

/**
  * Abstract syntax tree pretty-printing for Minijava.
  */
object PrettyPrinter extends PrettyPrinter
