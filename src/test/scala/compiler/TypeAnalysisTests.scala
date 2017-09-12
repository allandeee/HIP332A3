/**
  * Hipster cellular automaton language.
  * Type analysis tests.
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
 * Tests that verify that the type checker works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class TypeAnalysisTests extends SemanticTests {

  test ("check incorrect type of return expression in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { 
                |  float v = 2.1;
                |  return(v + 1); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 8, 10,
      "type error, expecting 'int' found 'float'")
  }

  test ("check incorrect type of return expression in function") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function test(int a) : neighbour { return(a); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 5, 43,
      "type error, expecting 'neighbour' found 'int'")
  }

  test ("check type coercion of return expression in function") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function test(int a) : float { return(a); }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test ("check handling of missing return values") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function test(int a) : float { return; }
                |updater {}
                |mapper { return; }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 5, 32, "missing return value")
    assertMessage(messages, 1, 7, 10, "missing return value")
  }
 
  // Tests associated with function identifiers.

  test("check assignment to a builtin identifier") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { fabs = 1.0; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 5, 11, "function identifier 'fabs' can only be used in a function call")
  }

  test("check assignment to a function identifier") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() {}
                |updater { f = 1.0; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 6, 11, "function identifier 'f' can only be used in a function call")
  }

  test("check use of function identifier in arithmetic expression") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() : boolean { return(true); }
                |updater { float i = f + 1.0; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 6, 21, "function identifier 'f' can only be used in a function call")
  }

  // Assignments

  test("check that values can be assigned to variables of any type") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  float a; int b; neighbour c; boolean d;
                |  a = 10; b = 1; c = me; d = true; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check that a type error in an rvalue expression doesn't also trigger an assignment type error") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  neighbour n;
                |  n = 10 + true; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 12, "type error, expecting 'int' or 'float' found 'boolean'")
  }

  // Function calls.

  test("check that only those identifiers declared as functions can be called") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int i) : float { return(i); }
                |initialiser test { }
                |updater { 
                |  int j = 0;
                |  int k = j(1,2);
                |  int l = test(k);
                |  float r = f(10); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 9, 11, "malformed function call: function name expected")
    assertMessage(messages, 1, 10, 11, "initialiser identifier 'test' can't be used here")
  }

  test("check reporting of incorrect argument numbers in function call") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int i, neighbour j) : float { return(i); }
                |function g() {  }
                |updater { 
                |  f(1); f(1, me); float i = f(1, me, 3.5);
                |  g(); g(1); g(1, me); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 8, 3, "malformed function call: argument count incorrect, expecting 2 found 1")
    assertMessage(messages, 1, 8, 29, "malformed function call: argument count incorrect, expecting 2 found 3")
    assertMessage(messages, 2, 9, 8, "malformed function call: argument count incorrect, expecting 0 found 1")
    assertMessage(messages, 3, 9, 14, "malformed function call: argument count incorrect, expecting 0 found 2")
  }

  test("check reporting of argument type errors in function call") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int i, neighbour j) : float { return(i); }
                |function g(boolean b) {  }
                |updater { 
                |  f(1, 1); float i = f(true, me); f(1, N);
                |  g(1); g(true); g(me);  }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 8, 8, "type error, expecting 'neighbour' found 'int'")
    assertMessage(messages, 1, 8, 24, "type error, expecting 'int' found 'boolean'")
    assertMessage(messages, 2, 9, 5, "type error, expecting 'boolean' found 'int'")
    assertMessage(messages, 3, 9, 20, "type error, expecting 'boolean' found 'neighbour'")
  }

  test("check reporting of incorrect argument numbers in call to builtin") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  cos(); float t = log(1.1, 1.1);
                |  fabs(); abs(1); int j = abs(1,true); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 6, 3, "malformed function call: argument count incorrect, expecting 1 found 0")
    assertMessage(messages, 1, 6, 20, "malformed function call: argument count incorrect, expecting 1 found 2")
    assertMessage(messages, 2, 7, 3, "malformed function call: argument count incorrect, expecting 1 found 0")
    assertMessage(messages, 3, 7, 27, "malformed function call: argument count incorrect, expecting 1 found 2")
  }

  test("check reporting of argument type errors in call to builtin") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  floor(N); int i = floor(true); floor(1.0);
                |  exp(2.6); exp(true); int j = round(1.1);  }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 6, 9, "type error, expecting 'int' or 'float' found 'neighbour'")
    assertMessage(messages, 1, 6, 27, "type error, expecting 'int' or 'float' found 'boolean'")
    assertMessage(messages, 2, 7, 17, "type error, expecting 'int' or 'float' found 'boolean'")
  }

  test("check the return types of function calls") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int i) : boolean { return(true); }
                |function g(int i) : neighbour { return(me); }
                |function h(int i) : int { return(1); }
                |function k(int i) : float { return(1.4); }
                |updater { 
                |  int i = f(0); boolean b = g(1);
                |  neighbour n = h(2); boolean t = k(3); }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 10, 11, "type error, expecting 'int' found 'boolean'")
    assertMessage(messages, 1, 10, 29, "type error, expecting 'boolean' found 'neighbour'")
    assertMessage(messages, 2, 11, 17, "type error, expecting 'neighbour' found 'int'")
    assertMessage(messages, 3, 11, 35, "type error, expecting 'boolean' found 'float'")
  }

  // Type checking of general expressions.

  test("verify type checking of correct arithmetic expressions") {
        val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  float y = 2.1;
                |  int i = 1 + 2 * 3 + 4 / 5 % 6 - 7;
                |  float j = 1.0 * 1.4 + 7.8 - -8.2 / 2.6 % -y;
                |  float k = 1.1 + 1 + 2 * y + i / j ; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("verify type checking of correct relational expressions") {
        val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |float y = 3.2;
                |state {}
                |updater { 
                |  int x = 22;
                |  boolean b = 1.1 + 2.3 <= y * 6.6;
                |  boolean c = me == N; 
                |  boolean d  = true > c;
                |  boolean e = 1.2 * 2.8 == x + 3;
                |  boolean f = y * 2.8 >= 1 + x;
                |  f = e == c; 
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("verify type checking of correct boolean expressions") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |int x = 10; boolean f = true;
                |state {}
                |updater { 
                |  float y;
                |  boolean a = !(x < 1.5) && f || (y == 1);
                |  boolean b = (a && y <= 1.3) || (x % 6 == 3) && f;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  // FIXME Add your tests here.

  // BEWARE the tests of correct type checking above actually pass
  // even when run against the framework compiler. This is a bit of a
  // surprise because the framework compiler really does no type checking
  // at all!

  // This happens largely because the existing cases in the `tipe` attribute
  // give most expresions the type `unknown`. Remeber, however, that we
  // reduce the continual re-reporting of type errors by not reporting them
  // for any expression whose inferred type is `unknown`.

  // So on their own these particular tests really aren't much of a test
  // of the effectiveness of the Hipster type checker. They only pick up
  // situations in which the type system incorrectly reports a type error
  // for an expression which should be type correct.

  // What about the other side of the coin? You should strongly consider
  // writing a bunch of tests to also make sure that your type checking
  // does pick up and correctly report the various kinds of type error that
  // can occur in an expression.

}

