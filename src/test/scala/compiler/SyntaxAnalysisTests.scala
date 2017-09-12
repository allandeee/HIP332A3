/**
  * Hipster cellular automaton language.
  * Tests of the Hipster parser.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

  import HipsterTree._

  val parsers = new SyntaxAnalysis (positions)
  import parsers._

  // Tests of numerical constant parsing.

  test("parsing decimal constant as a factor") {
    factor("34570") should
      parseTo[Expression] (IntExpr(34570))
  }

  test("parsing a hexadecimal constant as a factor") {
    factor("0xfffe") should
      parseTo[Expression] (IntExpr(65534))
  }

  test("parsing a binary constant as a factor") {
    factor("0B0101001") should
      parseTo[Expression] (IntExpr(41))
  }

  test("parsing an octal constant as a factor") {
    factor("0o777") should
      parseTo[Expression] (IntExpr(511))
  }

  test("parsing a float constant as a factor") {
    factor("10.22") should
      parseTo[Expression] (FloatExpr(10.22))
  }

  test("parsing a float constant with exponent as a factor") {
    factor("21e-5") should
      parseTo[Expression] (FloatExpr(0.00021))
  }

  test("parsing a float with exponent as a factor") {
    factor("0.123E6") should
      parseTo[Expression] (FloatExpr(123000))
  }

  // Negative constants.
  test("parsing a negative hexadecimal constant as a factor") {
    factor("-0x56") should
      parseTo[Expression] (NegExpr(IntExpr(86)))
  }

  test("parsing a negative float constant as a factor") {
    factor("-0.123e3") should
      parseTo[Expression] (NegExpr(FloatExpr(123)))
  }

  test("parsing a doubly negated constant as a factor") {
    factor("--20") should
      parseTo[Expression] (NegExpr(NegExpr(IntExpr(20))))
  }

  // A few tests of attempts to parse incorrect numeric constants.
  test("parsing a non-number as a integer constant gives an error") {
    intExpr("hello") should
      failParseAt(1, 1, "integer constant expected")
  }

  // A few tests of attempts to parse incorrect numeric constants.
  test("parsing a non-number as a floating point constant gives an error") {
    floatExpr("hello") should
      failParseAt(1, 1, "floating point constant expected")
  }

  test("parsing an overflowing integer constant gives an error") {
    factor("9999999999999999999") should
      failParseAt(1, 1, "integer will not fit into 32 bits")
  }

  test("parsing an overflowing floating point constant gives an error") {
    factor("1.56e27809") should
      failParseAt(1, 1, "floating point number is infinite")
  }

  test("parsing a very small floating point constant gives 0") {
    factor("1.56e-27809") should
      parseTo[Expression] (FloatExpr(0.0))
  }

  // Parsing identifiers
  test("parsing an identifier as a factor") {
    factor("doms_ident") should
      parseTo[Expression] (IdnExpr(IdnUse("doms_ident")))
  }

  test("parsing an identifier containing numerals") {
    factor("doms0_ident22") should
      parseTo[Expression] (IdnExpr(IdnUse("doms0_ident22")))
  }

  test("parsing a keyword as an identifier should fail") {
    factor("updater") should
      failParseAt(1, 1, "reserved word found where identifier expected")
  }

  test("parsing another keyword as an identifier should fail") {
    factor("then") should
      failParseAt(1, 1, "reserved word found where identifier expected")
  }

  test("parsing an identifier starting with a keyword is ok") {
    factor("function0") should
      parseTo[Expression] (IdnExpr(IdnUse("function0")))
  }

  // Parsing function calls and neighbour state expressions.
  test("parsing a neighbour expression as a factor") {
    factor("NE:height") should
    parseTo[Expression] (
      NeighbourExpr(IdnExpr(IdnUse("NE")),IdnUse("height")))
  }

  test("parsing a neighbour expression containing a keyword should fail") {
    factor("NE:state") should
      failParseAt(1, 4, "reserved word found where identifier expected")
  }

  test("parsing a neighbour exprssion containing a keyword should fail (2)") {
    factor("if:size") should
      failParseAt(1, 1, "reserved word found where identifier expected")
  }

  test("parsing a neighbour expr containing a non-identifier should fail") {
    phrase(factor)("NE:10ten") should
      failParseAt(1, 4, "identifier expected")
  }

  test("parsing a neighbour expr containing a non-identifier should fail (2)") {
    phrase(factor)("10:ten") should
      failParseAt(1, 3, "end of input expected")
  }

  test("parsing a function call as a factor") {
    factor("my_function(10,dom)") should
      parseTo[Expression] (FunCallExpr(IdnUse("my_function"),
        Vector(IntExpr(10),IdnExpr(IdnUse("dom")))))
  }

  test("parsing another function call as a factor") {
    factor("your_function(10+hello,goodbye)") should
      parseTo[Expression] (FunCallExpr(
        IdnUse("your_function"),
        Vector(
          PlusExpr(
            IntExpr(10),
            IdnExpr(IdnUse("hello"))),
          IdnExpr(IdnUse("goodbye")))))
  }

  test("parsing a simple arithmetic expression, checking precedence rules") {
    expression("2 + 3 * 4") should
    parseTo[Expression](
      PlusExpr(
        IntExpr(2),
        MultExpr(
          IntExpr(3),
          IntExpr(4))))
  }

  test("parsing a potentially ambiguous dangling else statement") {
    statement("if test then if test2 then count = 0; else count = 1;") should
      parseTo[Statement] (
        IfStmt(
          IdnExpr(IdnUse("test")),
          IfStmt(
            IdnExpr(IdnUse("test2")),
            AssignStmt(IdnExpr(IdnUse("count")),IntExpr(0)),
            Some(AssignStmt(IdnExpr(IdnUse("count")),IntExpr(1)))),
          None))
  }

  test("use a block statement to attach the else to the outer if"){
    statement(
      "if test then { if test2 then count = 0; } else count = 1;") should
      parseTo[Statement] (
        IfStmt(
          IdnExpr(IdnUse("test")),
          Block(
            Vector(
              IfStmt(
                IdnExpr(IdnUse("test2")),
                AssignStmt(IdnExpr(IdnUse("count")),IntExpr(0)),
                None))),
          Some(AssignStmt(IdnExpr(IdnUse("count")),IntExpr(1)))))
  }

  // Expression tests - unary operators

  test("parsing an expression containing a unary minus") {
    phrase(expression)("100 - -test * hello + - 20") should
    parseTo[Expression] (
      PlusExpr(
        MinusExpr(
          IntExpr(100),
          MultExpr(
            NegExpr(IdnExpr(IdnUse("test"))),
            IdnExpr(IdnUse("hello")))),
        NegExpr(IntExpr(20))))
  }

  test("parsing an expression containing a unary plus") {
    phrase(expression)("100 - +test * -(hello + +20)") should
    parseTo[Expression] (
        MinusExpr(
          IntExpr(100),
          MultExpr(
            IdnExpr(IdnUse("test")),
            NegExpr(
              PlusExpr(
                IdnExpr(IdnUse("hello")),
                IntExpr(20))))))
  }

  test("parsing expression starting with a unary minus") {
    phrase(expression)("- a + 20 * e") should
    parseTo[Expression] (
      PlusExpr(
        NegExpr(IdnExpr(IdnUse("a"))),
        MultExpr(
          IntExpr(20),
          IdnExpr(IdnUse("e")))))
  }

  test("parsing an expression containing a unary boolean not") {
    phrase(expression)("!true && !(a <= 10) || !(test && !hello)") should
    parseTo[Expression] (
      OrExpr(
        AndExpr(
          NotExpr(TrueExpr()),
          NotExpr(
            LessEqExpr(
              IdnExpr(IdnUse("a")),
              IntExpr(10)))),
        NotExpr(
          AndExpr(
            IdnExpr(IdnUse("test")),
            NotExpr(IdnExpr(IdnUse("hello")))))))
  }

  test("parsing an expression containing repeated unary minuses") {
    phrase(expression)("a - ---+-b") should
    parseTo[Expression] (
      MinusExpr(
        IdnExpr(IdnUse("a")),
        NegExpr(NegExpr(NegExpr(NegExpr(IdnExpr(IdnUse("b"))))))))
  }

  // Expression tests - arithmetic associatibity and precedence.
  test("parsing an add/sub expression to check associativity"){
    phrase(expression)("10 + a - 20 - b")
    parseTo[Expression] (
      MinusExpr(
        MinusExpr(
          PlusExpr(
            IntExpr(10),
            IdnExpr(IdnUse("a"))),
          IntExpr(20)),
        IdnExpr(IdnUse("b"))))
  }

  test("parsing a mult/div/mod expression to check associativity"){
    phrase(expression)("10 * a % 20 / b")
    parseTo[Expression] (
      MultExpr(
        ModExpr(
          DivExpr(
            IntExpr(10),
            IdnExpr(IdnUse("a"))),
          IntExpr(20)),
        IdnExpr(IdnUse("b"))))
  }

  test("parsing arithmetic expression to check precedence") {
    phrase(expression)("10 + a * 20 - c % 30 + d / 40 * 30")
    parseTo[Expression](
      PlusExpr(
        MinusExpr(
          PlusExpr(
            IntExpr(10),
            MultExpr(
              IdnExpr(IdnUse("a")),
              IntExpr(20))),
          ModExpr(
            IdnExpr(IdnUse("c")),
            IntExpr(30))),
        MultExpr(
          DivExpr(
            IdnExpr(IdnUse("d")),
            IntExpr(40)),
          IntExpr(30))))
  }

  // Expression tests - boolean and relational operators

  test("parsing an incorrect non-associative relational expression") {
    phrase(expression)("a <= b <= c") should
    failParseAt(1, 8, "'||' expected but '<' found")
  }

  test("parsing a conjunction of relational expressions") {
    phrase(expression)("a <= b && 100 > c") should
    parseTo[Expression] (
      AndExpr(
        LessEqExpr(
          IdnExpr(IdnUse("a")),
          IdnExpr(IdnUse("b"))),
        GreaterExpr(
          IntExpr(100),
          IdnExpr(IdnUse("c")))))
  }

  test("parsing relations between arithmetic expressions") {
    phrase(expression)("- 10 / a + 10 ==  c % a") should
    parseTo[Expression] (
      EqualExpr(
        PlusExpr(
          DivExpr(
            NegExpr(IntExpr(10)),
            IdnExpr(IdnUse("a"))),
          IntExpr(10)),
        ModExpr(
          IdnExpr(IdnUse("c")),
          IdnExpr(IdnUse("a")))))
  }

  test("parsing a boolean expression to check associativity") {
    phrase(expression)("a && !b || d >= 10 && c == true") should
    parseTo[Expression] (
      AndExpr(
        OrExpr(
          AndExpr(
            IdnExpr(IdnUse("a")),
            NotExpr(IdnExpr(IdnUse("b")))),
          GreaterEqExpr(
            IdnExpr(IdnUse("d")),
            IntExpr(10))),
        EqualExpr(
          IdnExpr(IdnUse("c")),
          TrueExpr())))
  }

  // You might expect an expression like `! a <= b` to parse as
  // `!(a <= b)`. The precedence list in the Hipster specification, however,
  // doesn't explicitly list the unary operator `!` so it must be included
  // in the "all other kinds of expression" clause and so it has a higher
  // binding precedence than `<=`. So under the specification
  // `! a <= b` should parse as (!a) <= b, and this behaviour is validated
  // by the following test.
  test("parsing an expression to check the relative precedence of ! and <=") {
    phrase(expression)("! a <= b") should
    parseTo[Expression] (
      LessEqExpr(
        NotExpr(IdnExpr(IdnUse("a"))),
        IdnExpr(IdnUse("b"))))
  }

  // Expression tests - bracketing.
  test("parsing an expression containing brackets (1)") {
    phrase(expression)("(10 + a) * ((20 - c) * (30))") should
    parseTo[Expression] (
      MultExpr(
        PlusExpr(
          IntExpr(10),
          IdnExpr(IdnUse("a"))),
        MultExpr(
          MinusExpr(
            IntExpr(20),
            IdnExpr(IdnUse("c"))),
          IntExpr(30))))
  }

  test("parsing and expression containing brackets (2)") {
    phrase(expression)("a || (true && (b && false))") should
    parseTo[Expression] (
      OrExpr(
        IdnExpr(IdnUse("a")),
        AndExpr(
          TrueExpr(),
          AndExpr(
            IdnExpr(IdnUse("b")),
            FalseExpr()))))
  }

  // Coordinate expressions
  test("parse an empty coordinate expression should fail") {
    phrase(coordExpr)("[]") should
    failParseAt(1, 2, "identifier or numerical constant expected")
  }

  test("parse a 1-d coordinate expression") {
    phrase(coordExpr)("[100 + test]") should
    parseTo[CoordExpr] (
      CoordExpr(Vector(
        PlusExpr(
          IntExpr(100),
          IdnExpr(IdnUse("test"))))))
  }

  test("parse a 3-d coordinate expression") {
    phrase(coordExpr)("[100 + test, bob, 100 % 3]") should
    parseTo[CoordExpr] (
      CoordExpr(Vector(
        PlusExpr(
          IntExpr(100),
          IdnExpr(IdnUse("test"))),
        IdnExpr(IdnUse("bob")),
        ModExpr(
          IntExpr(100),
          IntExpr(3)))))
  }

  // Function declarations

  test("parse function decl with empty parameter list and no return value") {
    phrase(topLevelDecl)("function test() { }") should
    parseTo[TopLevelDecl] (
      FunctionDecl(IdnDef("test"), Vector(), None, Vector()))
  }

  test("parse function decl with empty parameter list") {
    phrase(topLevelDecl)("function dom():int { return(0); }") should
    parseTo[TopLevelDecl] (
      FunctionDecl(IdnDef("dom"), Vector(), Some(IntType()),
        Vector(ReturnStmt(Some(IntExpr(0))))))
  }

  test("parse function decl with parameters but no return value") {
    phrase(topLevelDecl)("function trudy(int hello, boolean test) {  }") should
    parseTo[TopLevelDecl] (
      FunctionDecl(
        IdnDef("trudy"),
        Vector(
          ParamDecl(IntType(), IdnDef("hello")),
          ParamDecl(BoolType(), IdnDef("test"))),
        None, Vector()))
  }

  test("parse function decl with parameters and a return value") {
    phrase(topLevelDecl) (
      "function hamilton(int lib) : boolean { return(true); }") should
    parseTo[TopLevelDecl] (
      FunctionDecl(
        IdnDef("hamilton"),
        Vector(ParamDecl(IntType(), IdnDef("lib"))),
        Some(BoolType()),
        Vector(ReturnStmt(Some(TrueExpr())))))
  }

  // Neighbourhood declarations
  test("parse neighbourhood declaration with a single neighbour definition") {
    phrase(topLevelDecl)("neighbourhood N=[0,1];") should
    parseTo[TopLevelDecl] (
      NeighbourhoodDecl(
        Vector(
          NeighbourDecl(
            IdnDef("N"),
            CoordExpr(Vector(IntExpr(0),IntExpr(1)))))))
  }

  test("parse neighbourhood declaration with many neighbour definitions") {
    phrase(topLevelDecl)(
      "neighbourhood N=[0,1],S=[0,-1],W=[-1,0],E=[1,0];") should
    parseTo[TopLevelDecl] (
      NeighbourhoodDecl(
        Vector(
          NeighbourDecl(
            IdnDef("N"),
            CoordExpr(Vector(IntExpr(0),IntExpr(1)))),
          NeighbourDecl(
            IdnDef("S"),
            CoordExpr(Vector(IntExpr(0),NegExpr(IntExpr(1))))),
          NeighbourDecl(
            IdnDef("W"),
            CoordExpr(Vector(NegExpr(IntExpr(1)),IntExpr(0)))),
          NeighbourDecl(
            IdnDef("E"),
            CoordExpr(Vector(IntExpr(1),IntExpr(0)))))))
  }

  test("parse neighbourhood declaration with uninitialised definition") {
    phrase(topLevelDecl)(
      "neighbourhood N=[0,1],S,W=[-1,0],E=[1,0];") should
    failParseAt(1, 24, "'=' expected but ',' found")
  }

  test("parse a neighbourhood declaration with no neighbour definitions") {
    phrase(topLevelDecl)("neighbourhood ;") should
    failParseAt(1, 15, "identifier expected")
  }

  // For statement.
  test("parse a for statement with no step clause") {
    phrase(statement)(
      """for x = 1 to y
           z = z + x;""") should
    parseTo[Statement] (
      ForStmt(
        IdnDef("x"),
        IntExpr(1),
        IdnExpr(IdnUse("y")),
        None,
        AssignStmt(
          IdnExpr(IdnUse("z")),
          PlusExpr(
            IdnExpr(IdnUse("z")),
            IdnExpr(IdnUse("x"))))))
  }

  test("parse a for statement with a step clause") {
    phrase(statement) (
      """for x = 0 to 100 step 2
           z = z + x;""") should
    parseTo[Statement] (
      ForStmt(
        IdnDef("x"),
        IntExpr(0),
        IntExpr(100),
        Some(IntExpr(2)),
        AssignStmt(
          IdnExpr(IdnUse("z")),
          PlusExpr(
            IdnExpr(IdnUse("z")),
            IdnExpr(IdnUse("x"))))))
  }

  test("parse nested for statements") {
    phrase(statement)(
      """for x = 1 to width
           for y = 1 to height
             z = z + x + y;""") should
    parseTo[Statement](
      ForStmt(
        IdnDef("x"),
        IntExpr(1),
        IdnExpr(IdnUse("width")),
        None,
        ForStmt(
          IdnDef("y"),
          IntExpr(1),
          IdnExpr(IdnUse("height")),
          None,
          AssignStmt(
            IdnExpr(IdnUse("z")),
            PlusExpr(
              PlusExpr(
                IdnExpr(IdnUse("z")),
                IdnExpr(IdnUse("x"))),
              IdnExpr(IdnUse("y")))))))
  }

  // Iterate statement
  test("parse an iterate statement over all neighbours") {
    phrase(statement)(
      """iterate x over all
           count = count + 1;""") should
    parseTo[Statement] (
      IterateOverStmt(
        IdnDef("x"),
        All(),
        AssignStmt(
          IdnExpr(IdnUse("count")),
          PlusExpr(
            IdnExpr(IdnUse("count")),
            IntExpr(1)))))
  }

  test("parse an iterate statement over a set of neighbours") {
    phrase(statement)(
      """iterate x over [NE,SE,SW,NW]
           if alive then count = count + 1;""") should
    parseTo[Statement](
      IterateOverStmt(
        IdnDef("x"),
        Subset(Vector(
          IdnExpr(IdnUse("NE")),
          IdnExpr(IdnUse("SE")),
          IdnExpr(IdnUse("SW")),
          IdnExpr(IdnUse("NW")))),
        IfStmt(
          IdnExpr(IdnUse("alive")),
          AssignStmt(
            IdnExpr(IdnUse("count")),
            PlusExpr(
              IdnExpr(IdnUse("count")),
              IntExpr(1))),
        None)))
  }

  test("parse an iterate statement with a missing control variable") {
    phrase(statement)(
      """iterate over all
           x = x * 10;""") should
    failParseAt(1, 9, "reserved word found where identifier expected")
  }

  test("parse nested iterate statements"){
    phrase(statement)(
      """iterate x over others
           iterate y over [N,S,E,W]
             if (x:alive == y:alive) then
               count = count + 1;""") should
    parseTo[Statement](
      IterateOverStmt(
        IdnDef("x"),
        Others(),
        IterateOverStmt(
          IdnDef("y"),
          Subset(Vector(
            IdnExpr(IdnUse("N")),
            IdnExpr(IdnUse("S")),
            IdnExpr(IdnUse("E")),
            IdnExpr(IdnUse("W")))),
          IfStmt(
            EqualExpr(
              NeighbourExpr(IdnExpr(IdnUse("x")), IdnUse("alive")),
              NeighbourExpr(IdnExpr(IdnUse("y")), IdnUse("alive"))),
            AssignStmt(
              IdnExpr(IdnUse("count")),
              PlusExpr(IdnExpr(IdnUse("count")),IntExpr(1))),
            None))))
  }

  // Cell statement.
  test("parse a simple cell statement") {
    phrase(statement)(
      """cell [1,2] alive = true;""") should
    parseTo[Statement] (
      CellStmt(
        CoordExpr(Vector(
          IntExpr(1),IntExpr(2))),
        AssignStmt(
          IdnExpr(IdnUse("alive")),
          TrueExpr())))
  }

  test("parsing a cell statement with no coordinate") {
    phrase(statement)(
      """cell alive = true ;""") should
    failParseAt(1, 6,"'(' expected but 'a' found")
  }

  test("parsing a cell statement with an empty coordinate") {
    phrase(statement)(
      """cell [] alive = true ;""") should
    failParseAt(1,7,"identifier or numerical constant expected")
  }

  test("parsing a cell statement with a block body") {
    phrase(statement)(
      """cell [1,2,3] {
          count = 10;
          alive = false; }""") should
    parseTo[Statement](
      CellStmt(
        CoordExpr(Vector(IntExpr(1),IntExpr(2),IntExpr(3))),
        Block(Vector(
          AssignStmt(IdnExpr(IdnUse("count")),IntExpr(10)),
          AssignStmt(IdnExpr(IdnUse("alive")),FalseExpr())))))
  }
}


