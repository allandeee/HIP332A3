/**
  * This file is part of Kiama.
  * Adapted for the Hipster to Java translator.
  *
  * © 2012-2017 Anthony M Sloane, Macquarie University.
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  *
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

/**
  * MiniJava is a small subset of full Java which provides just enough
  * functionality to act as a suitable target for compilation
  * from Hipster.
  * 
  * This Module contains tree structures for representing MiniJava classes.
  */
object MiniJavaTree {

  import org.bitbucket.inkytonik.kiama.output.{
    Infix,
    LeftAssoc,
    Prefix,
    PrettyBinaryExpression,
    PrettyExpression,
    PrettyUnaryExpression
  }

  /**
    * The common supertype of all source tree nodes.
    */
  sealed abstract class MiniJavaNode extends Product

  /**
    * Abstract superclass of all top-level declarations in a class
    */
  sealed abstract class ClassMember extends MiniJavaNode

  /**
    * A general class with a given name, optional super class, and a 
    * possibly empty list of members.
    */
  case class Class(modifiers : Vector[Modifier], name : IdnDef,
    superclass : Option[IdnUse], members : Vector[ClassMember])
      extends ClassMember

  /**
    * A class field with a given type and name.
    */
  case class Field(modifiers : Vector[Modifier], tipe : Type,
    name : IdnDef, init : Option[Expression]) extends ClassMember

  /**
    * A method with an optional return type and access modifiers, name, 
    * list of arguments, and list of statements that comprise the 
    * method body.
    */
  case class Method(modifiers : Vector[Modifier], ret : ReturnSpec,
    name : IdnDef, args : Vector[ArgumentSpec], body : Vector[Statement]
  ) extends ClassMember

  /**
    * A static initialiser block
    */
  case class StaticInit(stmts : Vector[Statement]) extends ClassMember

  /**
    * Abstract superclass for return type specifications of 
    * methods and constructors.
    */
  abstract sealed class ReturnSpec extends MiniJavaNode

  /**
    * A void return specificastion, for methods with no return value.
    */
  case class Void() extends ReturnSpec 

  /**
    * A dummy return specification for constructors.
    */
  case class Construct() extends ReturnSpec

  /**
    * A dummy return specification for constructors
    * with a call to a super-class constructor.
    */
  case class ConstructWithSuper(args : Vector[Expression])
      extends ReturnSpec

  /**
    * A typed return specification.
    */
  case class ReturnType(tipe : Type) extends ReturnSpec

  /**
    * Abstract superclass for all argument specifications
    */
  sealed abstract class ArgumentSpec extends MiniJavaNode 

  /**
    * An argument with a given type and name.
    */
  case class Argument(tipe : Type, name : IdnDef) extends ArgumentSpec

  /**
    * A varargs argument with a given type and name.
    */
  case class Varargs(tipe : Type, name : IdnDef) extends ArgumentSpec

  /**
    * Common superclass for types.
    */
  abstract class Type extends MiniJavaNode

  /**
    * The basic integer type.
    */
  case class IntType() extends Type {
    override def toString() = "int"
  }

  /**
    * The basic Boolean type.
    */
  case class BooleanType() extends Type {
    override def toString() = "boolean"
  }

  /**
    * The basic double type.
    */
  case class DoubleType() extends Type {
    override def toString() = "double"
  }

  /**
    * A type given by the named class.
    */
  case class ClassType(name : IdnUse) extends Type {
    override def toString() = name.idn
  }

  /**
    * Array type.
    */
  case class ArrayType(t : Type) extends Type {
    override def toString() = t.toString + "[]"
  }

  /**
    * Common superclass for modifiers 
    * (public, private, static, final etc.)
    */
  sealed abstract class Modifier extends MiniJavaNode

  /**
    * Private modifier.
    */
  case class Private() extends Modifier  {
    override def toString = "private"
  }

  /**
    * Public modifier.
    */
  case class Public() extends Modifier {
    override def toString = "public"
  }

  /**
    * Final modifier.
    */
  case class Final() extends Modifier  {
    override def toString = "final"
  }

  /**
    * Static modifier.
    */
  case class Static() extends Modifier {
    override def toString = "static"
  }

  /**
    * Synchronized modifier.
    */
  case class Synchronized() extends Modifier {
    override def toString = "synchronized"
  }

  /**
    * Common superclass of statements.
    */
  sealed abstract class Statement extends MiniJavaNode

  /**
    * A block containing a possibly empty list of statements.
    */
  case class Block(stmts : Vector[Statement]) extends Statement

  /**
    * A variable declaraion with a given type, name and optional
    * initialiser expression.
    */
  case class Var(tipe : Type, name : IdnDef, init : Option[Expression])
      extends Statement

  /**
    * A return statement with optional return expression.
    */
  case class Return(exp : Option[Expression]) extends Statement

  /**
    * A conditional statement that tests the given expression, 
    * choosing `stmt1` if the expression is true, otherwise 
    * choosing `stmt2`.
    */
  case class If(exp : Expression, stmt1 : Statement,
    stmt2 : Statement) extends Statement

  /**
    * A while loop that tests the given expression and has as body 
    * the given statement.
    */
  case class While(exp : Expression, stmt : Statement) extends Statement

  /**
    * An assignment of the value of the given expression to the 
    * given lvalue expression.
    */
  case class Assign(name : LValue, exp : Expression) extends Statement

  /**
    * Method call statement. Execute the method with the given name called 
    * on the (optional) object given by the `base` expression with the 
    * given argument expressions. Any returned value is discarded.
    */
  case class Call(base : Option[Expression], name : IdnUse,
    args : Vector[Expression]) extends Statement

  /**
    * Empty statement. Does nothing.
    */
  case class Empty() extends Statement

  /**
    * Common superclass of expressions.
    */
  abstract class Expression extends MiniJavaNode with PrettyExpression

  /**
    * Common interface for binary expressions.
    */
  abstract class BinaryExpression(val op : String) extends Expression with PrettyBinaryExpression {
    def left : Expression
    def right : Expression
    val fixity = Infix(LeftAssoc)
  }

  /**
    * Common interface for unary expressions.
    */
  abstract class UnaryExpression(val op : String) extends Expression with PrettyUnaryExpression {
    def exp : Expression
    override val priority = 1
    val fixity = Prefix
  }

  /**
    * Boolean conjunction (AND) expression.
    */
  case class AndExp(left : Expression, right : Expression) extends BinaryExpression("&&") {
    override val priority = 6
  }

  /**
    * Boolean disjunction (OR) expression.
    */
  case class OrExp(left : Expression, right : Expression) extends BinaryExpression("||") {
    override val priority = 6
  }

  /**
    * Equals expression.
    */
  case class EqualsExp(left : Expression, right : Expression) extends BinaryExpression("==") {
    override val priority = 5
  }

  /**
    * Not equals expression.
    */
  case class NotEqualsExp(left : Expression, right : Expression) extends BinaryExpression("!=") {
    override val priority = 5
  }

  /**
    * Less than expression.
    */
  case class LessExp(left : Expression, right : Expression) extends BinaryExpression("<") {
    override val priority = 4
  }

  /**
    * Less than or equal expression.
    */
  case class LessEqExp(left : Expression, right : Expression) extends BinaryExpression("<=") {
    override val priority = 4
  }

  /**
    * Greater than expression.
    */
  case class GreaterExp(left : Expression, right : Expression) extends BinaryExpression(">") {
    override val priority = 4
  }

  /**
    * Greater than or equal expression.
    */
  case class GreaterEqExp(left : Expression, right : Expression) extends BinaryExpression(">=") {
    override val priority = 4
  }

  /**
    * Addition expression.
    */
  case class PlusExp(left : Expression, right : Expression) extends BinaryExpression("+") {
    override val priority = 3
  }

  /**
    * Subtraction expression.
    */
  case class MinusExp(left : Expression, right : Expression) extends BinaryExpression("-") {
    override val priority = 3
  }

  /**
    * Multiplication expression.
    */
  case class StarExp(left : Expression, right : Expression) extends BinaryExpression("*") {
    override val priority = 2
  }

  /**
    * Division expression.
    */
  case class DivExp(left : Expression, right : Expression) extends BinaryExpression("/") {
    override val priority = 2
  }

  /**
    * Modulus expression.
    */
  case class ModExp(left : Expression, right : Expression) extends BinaryExpression("%") {
    override val priority = 2
  }

  /**
    * Method call expression. Yield the value returned by the method with the
    * given name called on the (optional) object given by the `base` 
    * expression with the given argument expressions.
    */
  case class CallExp(base : Option[Expression], name : IdnUse,
    args : Vector[Expression]) extends Expression

  /**
    * Integer value expression.
    */
  case class IntExp(value : Int) extends Expression

  /**
    * Double value expression.
    */
  case class DoubleExp(value : Double) extends Expression

  /**
    * Boolean TRUE expression.
    */
  case class TrueExp() extends Expression

  /**
    * Boolean FALSE expression.
    */
  case class FalseExp() extends Expression

  /**
    * Lambda expression.
    */
  case class LambdaExp(args : Vector[ArgumentSpec], body : Statement)
      extends Expression

  /**
    * String expression.
    */
  case class StringExp(s : String) extends Expression

  /**
    * Boolean NOT expression.
    */
  case class NotExp(exp : Expression) extends UnaryExpression("!")

  /**
    * Negation expression.
    */
  case class NegExp(exp : Expression) extends UnaryExpression("-")

  /**
    * Abstract superclass for expressions that can occur on the 
    * left hand side of an assignment statement.
    */
  abstract sealed class LValue extends Expression

  /**
    * Identifier expression.
    */
  case class IdnExp(name : IdnUse) extends LValue

  /**
    * Array index epression. Refers to the `ind` element of the array
    * given by `base`.
    */
  case class IndExp(base : Expression, ind : Expression) extends LValue

  /**
    * Field expression, refers to the `fld` field of the object given
    * by `base`.
    */
  case class FieldExp(base : Expression, fld : IdnUse) extends LValue

  /**
    * THIS expression.
    * 
    * Ugghhhh! this isn't really an l-value, but I can't be bothered
    * to dance on the head of a pin at this juncture.
    */
  case class ThisExp() extends LValue

  /**
    * Instance creation expression. Yields a new instance of the given
    * class type.
    */
  case class NewExp(name : IdnUse, tail : NewTail) extends Expression

  /**
    * Abstract superclass for tail of new expression
    */
  abstract sealed class NewTail extends MiniJavaNode

  /**
    * Constructor argument list.
    */
  case class ConstructorArgs(args : Vector[Expression]) extends NewTail

  /**
    * Array dimension expressions.
    */
  case class ArrayDims(dims : Vector[Expression]) extends NewTail

  /**
    * A cast expression.
    */
  case class CastExp(cls : ClassType, exp : Expression) extends Expression

  /**
    * An identifier reference.
    */
  abstract class IdnTree extends MiniJavaNode {
    def idn : String
  }

  /**
    * A defining occurrence of an identifier.
    */
  case class IdnDef(idn : Identifier) extends IdnTree

  /**
    * An applied occurrence (use) of an identifier.
    */
  case class IdnUse(idn : Identifier) extends IdnTree

  /**
    * A representation of identifiers as strings.
    */
  type Identifier = String

}
