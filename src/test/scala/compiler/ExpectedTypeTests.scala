/**
  * Hipster cellular automaton language.
  * Type analysis, tests of expected type attribution.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests of expected type attribution.
  */
@RunWith(classOf[JUnitRunner])
class ExpectedTypeTests extends SemanticTests {

  // Test expected types of dimensions.

  test ("verify expected type of dimensions in a 'dimension' declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (true,false cyclic);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 2, 12, Set("int"))
    assertTypeErrorWithExpectedType(messages, 1, 2, 17, Set("int"))
  }

  // Tests of expected type of expression in top-level constant declaration

  test ("verify expected type of expression in 'float' constant decl is 'int' or 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float c = false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int", "float"))
  }

  test ("verify expected type of expression in 'int' constant decl is 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |int c = true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 9, Set("int"))
  }

  test ("verify expected type of expression in 'boolean' constant decl is 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean c = 0;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 13, Set("boolean"))
  }

  // Test expected type of expression in variable declarations.

  test ("verify expected type of expression in 'float' variable decl is 'int' or 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { float c = false; }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 4, 19, Set("int","float"))
  }

  test ("verify expected type of expression in 'int' variable decl is 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int c = true; }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 4, 17, Set("int"))
  }

  test ("verify expected type of expression in 'boolean' variable decl is 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { boolean c = 0; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 23, Set("boolean"))
  }

  test ("verify expected type of expression in 'neighbour' variable decl is 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { neighbour c = 0; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 25, Set("neighbour"))
  }

  // Tests of expected types on either side of an assignment statement

  test ("verify that the lvalue on the left of an assignment can have any type") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  boolean b; int c; float d; neighbour e;
                |  b = false;
                |  c = 10;
                |  d = 10.5;
                |  e = me; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test ("verify expected type of rvalue in assignment with lvalue of type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  boolean a;
                |  a = 10;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 7, 7, Set("boolean"))
  }

  test ("verify expected type of rvalue in assignment with lvalue of type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  int a;
                |  a = true;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 7, 7, Set("int"))
  }

  test ("verify expected type of rvalue in assignment with lvalue of type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  float a;
                |  a = true;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 7, 7, Set("int","float"))
    }

  test ("verify expected type of rvalue in assignment with lvalue of type 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  neighbour a;
                |  a = true;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 7, 7, Set("neighbour"))
  }

  // test expected type of control expression in an 'if' statement

  test ("verify expected type of control expression in 'if' is 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  if 10 then return;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 6, 6, Set("boolean"))
  }

  // test expected types of from, to and step expressions in a 'for' statement

  test ("verify expected type of from expression in 'for' stmt is 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                |initialiser dom {
                |  for x = true to 10 return;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 11, Set("int"))
  }

  test ("verify expected type of to expression in 'for' stmt is 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                |initialiser dom {
                |  for x = 0 to true return;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 16, Set("int"))
  }

  test ("verify expected type of step expression in 'for' stmt is 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                |initialiser dom {
                |  for x = 0 to 10 step true return;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 24, Set("int"))
  }

  // Verify expected type for 'return' statements.

  test ("verify expected type of 'return' expression in function with return type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() : int { return (true); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 30, Set("int"))
  }

  test ("verify expected type of 'return' expression in function with return type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() : boolean { return (10); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 34, Set("boolean"))
  }

  test ("verify expected type of 'return' expression in function with return type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() : float { return (true); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 32, Set("int","float"))
  }

  test ("verify expected type of 'return' expression in function with return type 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() : neighbour { return (true); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 36, Set("neighbour"))
  }

  // Tests of expected types for equals expressions

  test ("verify that the left hand operand of an equals expression can have any type") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true == false;
                |boolean c = 10 == 20;
                |boolean d = 10.1 == 20.1;
                |boolean e = me == N;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test ("verify expected type of right hand operand of an equals expression when its left hand has type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true == 10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("boolean"))
  }

  test ("verify expected type of right hand operand of an equals expression when its left hand has type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 == true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 19, Set("int","float"))
  }


  test ("verify expected type of right hand operand of an equals expression when its left hand has type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10.5 == true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("int","float"))
  }

  test ("verify expected type of right hand operand of an equals expression when its left hand has type 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = me == true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 19, Set("neighbour"))
  }

  // Tests of expected types for comparison relation < expressions

  test ("verify that the left hand operand of a < expression can have any type except 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true < false;
                |boolean c = 10 < 20;
                |boolean d = 10.1 < 20.1;
                |boolean e = me < N;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 13, Set("int","float","boolean"))
  }

  test ("verify expected type of right hand operand of a < expression when its left hand has type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true < 10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 20, Set("boolean"))
  }

  test ("verify expected type of right hand operand of a < expression when its left hand has type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 < true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 18, Set("int","float"))
  }

  test ("verify expected type of right hand operand of a < expression when its left hand has type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10.5 < true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 20, Set("int","float"))
  }

  // Tests of expected types for comparison relation > expressions

  test ("verify that the left hand operand of a > expression can have any type except 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true > false;
                |boolean c = 10 > 20;
                |boolean d = 10.1 > 20.1;
                |boolean e = me > N;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 13, Set("int","float","boolean"))
  }

  test ("verify expected type of right hand operand of a > expression when its left hand has type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true > 10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 20, Set("boolean"))
  }

  test ("verify expected type of right hand operand of a > expression when its left hand has type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 > true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 18, Set("int","float"))
  }

  test ("verify expected type of right hand operand of a > expression when its left hand has type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10.5 > true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 20, Set("int","float"))
  }

  // Tests of expected types for comparison relation <= expressions

  test ("verify that the left hand operand of a <= expression can have any type except 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true <= false;
                |boolean c = 10 <= 20;
                |boolean d = 10.1 <= 20.1;
                |boolean e = me <= N;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 13, Set("int","float","boolean"))
  }

  test ("verify expected type of right hand operand of a <= expression when its left hand has type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true <= 10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("boolean"))
  }

  test ("verify expected type of right hand operand of a <= expression when its left hand has type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 <= true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 19, Set("int","float"))
  }

  test ("verify expected type of right hand operand of a <= expression when its left hand has type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10.5 <= true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("int","float"))
  }

  // Tests of expected types for comparison relation >= expressions

  test ("verify that the left hand operand of a >= expression can have any type except 'neighbour'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true >= false;
                |boolean c = 10 >= 20;
                |boolean d = 10.1 >= 20.1;
                |boolean e = me >= N;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 13, Set("int","float","boolean"))
  }

  test ("verify expected type of right hand operand of a >= expression when its left hand has type 'boolean'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = true >= 10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("boolean"))
  }

  test ("verify expected type of right hand operand of a >= expression when its left hand has type 'int'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 >= true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 19, Set("int","float"))
  }

  test ("verify expected type of right hand operand of a >= expression when its left hand has type 'float'") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10.5 >= true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 21, Set("int","float"))
  }

  // Tests of expected types for operands of arithmetic operators

  test ("verify expected type of operands to a + expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = true + false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 18, Set("int","float"))
  }

  test ("verify expected type of operands to a - expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = true - false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 18, Set("int","float"))
  }

  test ("verify expected type of operands to a * expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = true * false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 18, Set("int","float"))
  }

  test ("verify expected type of operands to a / expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = true / false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 18, Set("int","float"))
  }

    test ("verify expected type of operands to a % expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = true % false;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 11, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 18, Set("int","float"))
  }

    test ("verify expected type of operand of a unary - expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |float b = -true;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 12, Set("int","float"))
  }

  // Tests of expected types for operands of boolean operators

  test ("verify expected type of operands to a && expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 && 20.1;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 13, Set("boolean"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 19, Set("boolean"))
  }

  test ("verify expected type of operands to a || expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = 10 || 20.1;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 5, 13, Set("boolean"))
    assertTypeErrorWithExpectedType(messages, 1, 5, 19, Set("boolean"))
  }

  test ("verify expected type of operand of a unary ! expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |boolean b = !10;
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 5, 14, Set("boolean"))
  }

  // Tests of expected types of arguments in a function call.

  test ("verify expected type of argument in a call to a single parameter function") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int a) { }
                |updater { f(true); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 6, 13, Set("int"))
  }

  test ("verify expected type of all arguments in a call to a four parameter function") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int a, float b, boolean c, neighbour d) { }
                |updater { f(true, true, 10, true); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertTypeErrorWithExpectedType(messages, 0, 6, 13, Set("int"))
    assertTypeErrorWithExpectedType(messages, 1, 6, 19, Set("int","float"))
    assertTypeErrorWithExpectedType(messages, 2, 6, 25, Set("boolean"))
    assertTypeErrorWithExpectedType(messages, 3, 6, 29, Set("neighbour"))
  }

  // Test expected type of left hand operand of a neighbour state expression

  test ("verify expected type of left hand operand to the : operator") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |int i = 10;
                |updater { boolean b = i:alive; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 6, 23, Set("neighbour"))
  }

  // Tests of expected type for ordinates in a coordinate expression

  test ("verify expected type of ordinate in a 1-dimensional coordinate expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100);
                |neighbourhood P = [true];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 3, 20, Set("int"))
  }

  test ("verify expected types of ordinates in a 2-dimensional coordinate expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood P = [true,1.1];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 3, 20, Set("int"))
    assertTypeErrorWithExpectedType(messages, 1, 3, 25, Set("int"))
  }

  // Tests of expected type of identidfiers in an subset clause of an
  // iterate...over statement.

  test ("verify expected type of identifer in single entry neighbour subset") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |int i = 10;
                |updater {
                |  boolean b;
                |  iterate x over [i] b = x:alive;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertTypeErrorWithExpectedType(messages, 0, 8, 19, Set("neighbour"))
  }

  test ("verify expected type of identifers in multiple entry neighbour subset") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |int i = 10;
                |updater {
                |  boolean b;
                |  iterate x over [i, N, i] b = x:alive;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertTypeErrorWithExpectedType(messages, 0, 8, 19, Set("neighbour"))
    assertTypeErrorWithExpectedType(messages, 1, 8, 25, Set("neighbour"))
  }
}
