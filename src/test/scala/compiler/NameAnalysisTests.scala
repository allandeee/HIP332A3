/**
  * Hipster cellular automaton language.
  * Name analysis tests.
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
 * Tests that check that the name analysis works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class NameAnalysisTests extends SemanticTests {

  // Tests of multiple declaration.

  test("check multiple declaration in a state block") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0;
                |  boolean test; }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 6, 11, "'test' is declared more than once in current scope")
  }

  test("check multiple declaration in a `neighbourhood` declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], N = [1,0];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 3, 26, "'N' is declared more than once in current scope")
  }


  test("check re-declaration of neighbour identifier in `state` declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int N = 10; }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 4, 13, "'N' is declared more than once in current scope")
  }

  test("check re-declaration of constant identifiers in `state` and `neighbourhood` declarations") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |int N = 100;
                |boolean alive = false;
                |neighbourhood N = [0,1];
                |state { int alive = 10; }
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 5, 15, "'N' is declared more than once in current scope")
    assertMessage(messages, 1, 6, 13, "'alive' is declared more than once in current scope")
  }

  test("check redeclaration of state field identifier in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0; }
                |updater {
                |  boolean test; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check redeclaration of state field identifier in mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0; }
                |updater { }
                |mapper { 
                |  boolean test;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check redeclaration of state field identifier in initialiser") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0; }
                |updater { }
                |mapper { 
                |  return(0); }
                |initialiser fly {
                |  boolean test; }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check redeclaration of state field identifier in function body") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0; }
                |function fly() : boolean {
                |  boolean test;
                |  return(test); }
                |updater { }
                |mapper { 
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check redeclaration of variables in a block statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  int test = 0;
                |  { test = 10; int test = 5; boolean test = true; }
                |}
                |mapper { 
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 38, "'test' is declared more than once in current scope")
  }

  // Tests of the scoping of variables

  test ("check that various builtins are defined in the global scope and have the correct types") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  int t = atan(1.1);
                |  int l = log10(10.2); }
                |mapper {
                |  int e = exp(4.5);
                |  int c = cos(10.1); 
                |  return(abs(floor(c))); }
                |initialiser blurg {
                |  int f = frnd();
                |  int r = round(-frnd());
                |}
                """.stripMargin)
    assert(messages.length === 5, "expecting five errors")
    // checking return types using the error that should be generated by
    // assigning a 'float' return value to an 'int' variable.
    assertMessage(messages, 0, 6, 11, "type error, expecting 'int' found 'float'")
    assertMessage(messages, 1, 7, 11, "type error, expecting 'int' found 'float'")
    assertMessage(messages, 2, 9, 11, "type error, expecting 'int' found 'float'")
    assertMessage(messages, 3, 10, 11, "type error, expecting 'int' found 'float'")
    assertMessage(messages, 4, 13, 11, "type error, expecting 'int' found 'float'")
  }

  test("check that state variables and neighbour constants are visible in updater and mapper blocks") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |updater { alive = N:alive; }
                |mapper { if alive then return(1); else return(1); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check that variables declared in function don't escape body scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { }
                |function fly() : boolean {
                |  boolean test = false;
                |  return(test); }
                |updater { test = true; }
                |mapper { 
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 8, 11, "'test' is not declared at this point")
  }

  test("check that variables declared in updater don't escape body scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { }
                |updater { 
                |  boolean test = false; 
                |  test = true; }
                |mapper { 
                |  if test then
                |    return(0);
                |  else
                |    return(1); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 9, 6, "'test' is not declared at this point")
  }

  test("check that variables declared in a `block` statement don't escape that scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  {
                |    int test = 0;
                |    test = test + 1;
                |  } 
                |  test = 10; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 10, 3, "'test' is not declared at this point")
  }

  test("check that `block` statement body is a new scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  boolean test = false;
                |  {
                |    int test = 0;
                |    test = test + 1;
                |  } 
                |  test = true; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  // Function declarations

  test("check that function only in scope at points after its declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |int b = f(0);
                |state {}
                |int c = f(2);
                |function f(int x) : int { return(x * 2); }
                |int d = f(4);
                |updater { int e = f(6); }
                |mapper { return(f(8)); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 4, 9, "'f' is not declared at this point")
    assertMessage(messages, 1, 6, 9, "'f' is not declared at this point")
  }

  test("check that function name is not in scope in the body of its own declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f() { f(); }
                |updater { }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 5, 16, "'f' is not declared at this point")
  }

  test("check that parameters of a function are in scope in its body") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int x, float y, boolean b, neighbour n) { 
                |   int c = floor(y); y = x; b = !b; neighbour m = n; }
                |updater { }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  test("check that parameters of a function do not leak into enclosing scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |function f(int x, float y, boolean b, neighbour n) {}
                |int x = 0; float y = 1; boolean b = false;
                |updater { }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "expecting no errors")
  }

  // Initialiser declarations

  test("check that initialiser name is not defined within its own body") {
    val messages = 
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |mapper { return(0); }
                |initialiser test { int a = test; }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 28, "'test' is not declared at this point")
  }

  test("check that a multiple definition error is emitted by an initialiser declaration") {
    val messages = 
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |int test = 0;
                |mapper { return(0); }
                |initialiser test { int a = test; }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 8, 13, "'test' is declared more than once in current scope")
  }

  test("check use of initialiser name as an identifier in expressions or assignments") {
    val messages = 
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |initialiser test { }
                |updater { test = 20; int temp = 100 * test + 10; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 6, 11, "initialiser identifier 'test' can't be used here")
    assertMessage(messages, 1, 6, 39, "initialiser identifier 'test' can't be used here")
  }

  // Check scoping of control variable and expressions in `for` loop.

  test("check that 'from' expression is in enclosing scope of `for` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |mapper { return(0); }
                |initialiser main {
                |  int i = 0; 
                |  for x = x + i to 10
                |    i = x;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 9, 11, "'x' is not declared at this point")
  }

  test("check that 'to' expression is in enclosing scope of `for` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |mapper { return(0); }
                |initialiser main {
                |  int i = 0; 
                |  for x = 0 to x + i
                |    i = x;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 9, 16, "'x' is not declared at this point")
  }

  test("check that 'step' expression is in enclosing scope of `for` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |mapper { return(0); }
                |initialiser main {
                |  int i = 0; 
                |  for x = 0 to 10 step x + i
                |    i = x;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 9, 24, "'x' is not declared at this point")
  }

  test("check that control variable doesn't leak into scope enclosing `for` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |mapper { return(0); }
                |initialiser main {
                |  int i = 0; 
                |  for x = 0 to 10
                |    i = x;
                |  x = 5;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 11, 3, "'x' is not declared at this point")
  }

  // Check scoping of control variable in `iterate` loop.

  test("check that control variable doesn't leak into scope enclosing `iterate` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  neighbour t = me;
                |  iterate x over [N, S]
                |    t = x;
                |  x = N;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 9, 3, "'x' is not declared at this point")
  }

  test("check that neighbour subset is in enclosing scope of `iterate` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  neighbour t = me;
                |  iterate x over [N, S, x]
                |    t = x;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 25, "'x' is not declared at this point")
  }

  test("check that neighbour subset in `iterate` statement can't contain a variable") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  neighbour t = me;
                |  iterate x over [N, S, t, me]
                |    t = x;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 25, "non-constant identifier in neighbour subset")
  }

  // Test to make sure that variable declarations are always enclosed in
  // blocks (surrounded by curly braces) in the bodies of various constructs.

  test("check that variable declarations in a `then` or `else` clause of an `if` statement must be enclosed in a block") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {
                |  int test = 0; }
                |updater {
                |  if test == 10 then int temp = 0;
                |  else { boolean temp = false; }
                |  if test == 10 then { int temp = 0; }
                |  else boolean temp = false; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 7, 22, "variable declarations must be enclosed in a block in the 'then' or 'else' clause of an 'if' statement")
    assertMessage(messages, 1, 10, 8, "variable declarations must be enclosed in a block in the 'then' or 'else' clause of an 'if' statement")
  }

  test("check that a variable declaration in the body of a `cell` statement must be enclosed in a block") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |initialiser first {
                |  cell [0,0] boolean temp;
                |  cell [21,2] { int temp; }
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 14, "variable declarations must be enclosed in a block in the body of a 'cell' statement")
  }

    test("check that a variable declaration in the body of a `iterate...over` statement must be enclosed in a block") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { 
                |  iterate x over all int test = 0;
                |  iterate y over [N, me] { boolean temp = false; }
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 6, 22, "variable declarations must be enclosed in a block in the body of an 'iterate...over' statement")
  }

  test("check that a variable declaration in the body of a `for` statement must be enclosed in a block") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater { }
                |initialiser first {
                |  for i = 1 to 100 boolean temp;
                |  for x = 10 to 1 step -1 { int temp = 0; }
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 20, "variable declarations must be enclosed in a block in the body of a 'for' statement")
  }

  // Tests of constant and variable declarations.

  test ("check that initialiser expression of variable declaration is evaluated in enclosing scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |updater {
                |  int x = x * 10; 
                |  int y = count % 3;
                |  y = y + 3;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 6, 11, "'x' is not declared at this point")
  }

  test ("check that initialiser expression of constant declaration is evaluated in enclosing scope") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |int width = width + 1;
                |int height = 100;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |updater {
                |  count = height / 7; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 3, 13, "'width' is not declared at this point")
  }


}

