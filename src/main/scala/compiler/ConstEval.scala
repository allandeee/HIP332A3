/**
  * Hipster cellular automaton language.
  * Constant expression evaluator for the Hipster language.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

object ConstEval {

  abstract sealed class ConstVal
  case class IntVal(i : Int) extends ConstVal
  case class FloatVal(v : Double) extends ConstVal
  case class BoolVal(b : Boolean) extends ConstVal
  case class NeighbourVal(c : Vector[Int]) extends ConstVal {
    def dim : Int = c.length
    def isMe : Boolean = c.forall(_ == 0)
  }
  case class UnknownVal() extends ConstVal

  def plus(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => IntVal(i + j)
          case FloatVal(w) => FloatVal(i + w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => FloatVal(v + j)
          case FloatVal(w) => FloatVal(v + w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def minus(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => IntVal(i - j)
          case FloatVal(w) => FloatVal(i - w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => FloatVal(v - j)
          case FloatVal(w) => FloatVal(v - w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def mult(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => IntVal(i * j)
          case FloatVal(w) => FloatVal(i * w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => FloatVal(v * j)
          case FloatVal(w) => FloatVal(v * w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def div(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => IntVal(i / j)
          case FloatVal(w) => FloatVal(i / w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => FloatVal(v / j)
          case FloatVal(w) => FloatVal(v / w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def mod(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => IntVal(i % j)
          case FloatVal(w) => FloatVal(i % w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => FloatVal(v % j)
          case FloatVal(w) => FloatVal(v % w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def neg(n : ConstVal) : ConstVal =
    n match {
      case IntVal(i) => IntVal(-i)
      case FloatVal(v) => FloatVal(-v)
      case _ => UnknownVal()
    }

  def and(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case BoolVal(b) =>
        r match {
          case BoolVal(c) => BoolVal(b && c)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def or(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case BoolVal(b) =>
        r match {
          case BoolVal(c) => BoolVal(b || c)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def not(n : ConstVal) : ConstVal =
    n match {
      case BoolVal(b) => BoolVal(!b)
      case _ => UnknownVal()
    }

  def equal(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => BoolVal(i == j)
          case FloatVal(w) => BoolVal(i == w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => BoolVal(v == j)
          case FloatVal(w) => BoolVal(v == w)
          case _ => UnknownVal()
        }
      case BoolVal(b) =>
        r match {
          case BoolVal(c) => BoolVal(b == c)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def less(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => BoolVal(i < j)
          case FloatVal(w) => BoolVal(i < w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => BoolVal(v < j)
          case FloatVal(w) => BoolVal(v < w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def greater(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => BoolVal(i > j)
          case FloatVal(w) => BoolVal(i > w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => BoolVal(v > j)
          case FloatVal(w) => BoolVal(v > w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def lesseq(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => BoolVal(i <= j)
          case FloatVal(w) => BoolVal(i <= w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => BoolVal(v <= j)
          case FloatVal(w) => BoolVal(v <= w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }

  def greatereq(l : ConstVal, r : ConstVal) : ConstVal =
    l match {
      case IntVal(i) =>
        r match {
          case IntVal(j) => BoolVal(i >= j)
          case FloatVal(w) => BoolVal(i >= w)
          case _ => UnknownVal()
        }
      case FloatVal(v) =>
        r match {
          case IntVal(j) => BoolVal(v >= j)
          case FloatVal(w) => BoolVal(v >= w)
          case _ => UnknownVal()
        }
      case _ => UnknownVal()
    }
}
