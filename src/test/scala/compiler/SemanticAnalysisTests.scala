/**
  * Hipster cellular automaton language.
  * General semantic analysis tests.
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
  * Tests that check the general behaviour of the semantic analyser.
  * These largely check the reporting of semantic errors that do not
  * arise directly from name analysis or type checking.
  */
@RunWith(classOf[JUnitRunner])
class SemanticAnalysisTests extends SemanticTests {

  // Just about the simplest possible correct Hipster program

  test ("minimal correct hipster program") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 0, "wasn't expecting any errors")
  }

  // Tests involving missing or repeated top level declarations

  test ("missing `state` declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting a single error")
    assertMessage (messages, 0, 2, 1, "missing 'state' declaration")
  }

  test("repeated `updater` declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                |updater {}
                """.stripMargin)
    assert(messages.length === 1, "expecting a single error")
    assertMessage(messages, 0, 7, 1, "repeated 'updater' declaration")
  }

  test ("missing `dimension` declaration") {
    val messages =
      semanticTestInline ("""
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 2, 1, "missing 'dimension' declaration")
    assertMessage(messages, 1, 2, 19, "dimension not declared")
  }

  test ("missing neighbourhood declaration") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |state {}
                |updater {}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting a single error")
    assertMessage(messages, 0, 2, 1, "missing 'neighbourhood' declaration")
  }

  // Test assignment to constants, neighbour symbols and control variables

  test("check assignment to constant") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  const = false; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 3, "can't assign to a constant or control variable")
  }

  test("check assignment to neighbour constant") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  N = S; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 3, "can't assign to a constant or control variable")
  }

  test("check assignment to `me` neighbour constant") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state {}
                |updater {
                |  me = N; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 7, 3, "can't assign to a constant or control variable")
  }

  test("check assignment to control variable of `for` loop") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {}
                |mapper { return(0); }
                |initialiser solitary {
                |  for i = 1 to 10
                |    i = 40;
                |}
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 10, 5, "can't assign to a constant or control variable")
  }

  test("check assignment to control variable of `iterate...over` statement") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state {}
                |updater {
                |  iterate x over all
                |    x = N; }
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting one error")
    assertMessage(messages, 0, 8, 5, "can't assign to a constant or control variable")
  }

  // Tests of errors that arise from the inappropriate use of `for`, `cell`
  // and assigment statements in updater.

  test("check use of `for` in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |updater {
                |  { for j = -10 to 10 count = count + 1; }
                |  for k = 0 to 10 count = count - 1;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 7, 5, "updater body can't contain a `for` statement")
    assertMessage(messages, 1, 8, 3, "updater body can't contain a `for` statement")

  }

  test("check indirect use of `for` in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f() { 
                |  int i = 0;
                |  for x = 1 to 10
                |    i = x * 2;
                |}
                |function g() {}
                |function h() : int { f(); return(10); }
                |updater {
                |  f();
                |  g();
                |  int i = h() + 100;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 14, 3, "updater can't call an initialiser function")
    assertMessage(messages, 1, 16, 11, "updater can't call an initialiser function")
  }

    test("check use of `cell` in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |updater {
                |  { cell [1,1] count = count + 1; }
                |  cell [1,1] count = count - 1;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 7, 5, "updater body can't contain a `cell` statement")
    assertMessage(messages, 1, 8, 3, "updater body can't contain a `cell` statement")

  }

  test("check indirect use of `cell` in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f() { 
                |  cell [1,1] count = 0; 
                |}
                |function g() {}
                |function h() : int { f(); return(10); }
                |updater {
                |  f();
                |  g();
                |  int i = h() + 100;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 12, 3, "updater can't call an initialiser function")
    assertMessage(messages, 1, 14, 11, "updater can't call an initialiser function")
  }

  test("check direct and indirect call to tainted built-in in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f() { count = floor(frnd()); }
                |updater {
                |  count = rnd(10);
                |  f();
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 7, 11, "updater can't call a tainted function")
    assertMessage(messages, 1, 8, 3, "updater can't call a tainted function")
  }

  test("check non-me state assignment in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state { int count = 0; }
                |updater {
                |  neighbour anotherme = S;
                |  { 
                |    N:count = 10;       // write to neighbour, fail 
                |  }
                |  anotherme:count = 30; 
                |      // variable anotherme may not contain me, fail
                |  iterate c over all
                |    c:count = 20;
                |      // variable c may not contain me, fail
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 9, 5, "updater can't assign to state of neighbour cell")
    assertMessage(messages, 1, 11, 3, "updater can't assign to state of neighbour cell")
    assertMessage(messages, 2, 14, 5, "updater can't assign to state of neighbour cell")
  }

  test("check indirect use of non-me state assignment in updater") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f(neighbour n) { 
                |  n:count = 0;
                |}
                |function g() { f(me); }
                |function h() : int { N:count = 10; return(10); }
                |updater {
                |  f(N);
                |  g();
                |  int i = h() + 100;
                |}
                |mapper { return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 11, 3, "function called by updater makes illegal state access")
    assertMessage(messages, 1, 12, 3, "function called by updater makes illegal state access")
    assertMessage(messages, 2, 13, 11, "function called by updater makes illegal state access")
  }

  // Tests of errors that arise from the inappropriate use of `for`, `cell`
  // and assigment statements in colour mapper.

  test("check use of `for` in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper { 
                |  int count = 0;
                |  { for j = -10 to 10 count = count + 1; }
                |  for k = 0 to 10 count = count - 1;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 9, 5, "colour mapper body can't contain a `for` statement")
    assertMessage(messages, 1, 10, 3, "colour mapper body can't contain a `for` statement")
  }

  test("check indirect use of `for` in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f() { 
                |  int i = 0;
                |  for x = 1 to 10
                |    i = x * 2;
                |}
                |function h() : int { f(); return(10); }
                |updater {}
                |mapper { 
                |  f();
                |  int i = h() + 100;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 14, 3, "colour mapper can't call an initialiser function")
    assertMessage(messages, 1, 15, 11, "colour mapper can't call an initialiser function")
  }

    test("check use of `cell` in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state {}
                |updater {}
                |mapper {
                |  int count = 0;  
                |  { cell [1,1] count = count + 1; }
                |  cell [1,1] count = count - 1;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 9, 5, "colour mapper body can't contain a `cell` statement")
    assertMessage(messages, 1, 10, 3, "colour mapper body can't contain a `cell` statement")
  }

  test("check indirect use of `cell` in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state {}
                |function f() { 
                |  cell [1,1] { int count = 0; } 
                |}
                |function h() : int { f(); return(10); }
                |updater {}
                |mapper { 
                |  f();
                |  int i = h() + 100;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 12, 3, "colour mapper can't call an initialiser function")
    assertMessage(messages, 1, 13, 11, "colour mapper can't call an initialiser function")
  }

  test("check state assignment in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state { int count = 0; }
                |updater {}
                |mapper { 
                |  { N:count = 10; me:count = 0; }
                |  count = 30;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 8, 5, "colour mapper can't write to the state of any cell")
    assertMessage(messages, 1, 8, 19, "colour mapper can't write to the state of any cell")
    assertMessage(messages, 2, 9, 3, "colour mapper can't write to the state of any cell")
  }

  test("check indirect use of state assignment in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1];
                |state { int count = 0; }
                |function f() { 
                |  cell [1,1] count = 0; 
                |}
                |function h() : int { count = 10; return(10); }
                |updater {}
                |mapper { 
                |  f();
                |  int i = h() + 100;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 12, 3, "colour mapper can't call an initialiser function")
    assertMessage(messages, 1, 12, 3, "function called by colour mapper makes illegal state access")
    assertMessage(messages, 2, 13, 11, "function called by colour mapper makes illegal state access")
  }

  test("check state access in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state { int count = 0; }
                |updater {}
                |mapper { 
                |  int temp = 0;
                |  { temp = N:count; }
                |  temp = (count + 30) % 20;
                |  temp = (S:count + 40) * 20;
                |  iterate x over others temp = temp + x:count;
                |  return(0); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 9, 12, "colour mapper can't read state of neighbour cell")
    assertMessage(messages, 1, 11, 11, "colour mapper can't read state of neighbour cell")
    assertMessage(messages, 2, 12, 39, "colour mapper can't read state of neighbour cell")
  }

  test("check indirect state access in colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |boolean const = true;
                |neighbourhood N = [0,1], S = [0,-1];
                |state { int count = 0; }
                |updater {}
                |function f(neighbour p) { int temp = p:count; }
                |function g() { f(me); }
                |function h() : int { return(S:count); }
                |function k() : int { neighbour n = me; return(n:count); }
                |mapper { 
                |  f(me);
                |  g();
                |  return(10 + h() * 20 * k()); }
                """.stripMargin)
    assert(messages.length === 4, "expecting four errors")
    assertMessage(messages, 0, 12, 3, "function called by colour mapper makes illegal state access")
    assertMessage(messages, 1, 13, 3,  "function called by colour mapper makes illegal state access")
    assertMessage(messages, 2, 14, 15,  "function called by colour mapper makes illegal state access")
    assertMessage(messages, 3, 14, 26,  "function called by colour mapper makes illegal state access")
  }

  test("check direct and indirect call to tainted built-in from colour mapper") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |function f() : int { return (floor(frnd())); }
                |updater {}
                |mapper { 
                |  if alive then
                |    return (rnd(10));
                |  else
                |    return (f());
                |}
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 9, 13, "colour mapper can't call a tainted function")
    assertMessage(messages, 1, 11, 13, "colour mapper can't call a tainted function")
  }

  // Tests of errors that arise from access to state from outside of
  // the scope of a `cell` statement in an initialiser.

  test("check access to state outside of `cell` statement in initialiser") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state { boolean alive = false; }
                |updater {}
                |mapper { return(0); }
                |initialiser scamp {
                |  alive = S:alive;
                |  N:alive = true; }
                """.stripMargin)
    assert(messages.length === 5, "expecting five errors")
    assertMessage(messages, 0, 8, 3, "initialiser can't access state outside of the body of a `cell` statement")
    assertMessage(messages, 1, 8, 11, "initialiser can't reference neighbour symbol outside of the body of a `cell` statement")
    assertMessage(messages, 2, 8, 13, "initialiser can't access state outside of the body of a `cell` statement")
    assertMessage(messages, 3, 9, 3, "initialiser can't reference neighbour symbol outside of the body of a `cell` statement")
    assertMessage(messages, 4, 9, 5, "initialiser can't access state outside of the body of a `cell` statement")
  }

  test("check indirect access to state outside of `cell` statement in initialiser") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state { boolean alive = false; }
                |updater {}
                |mapper { return(0); }
                |function f() {
                |  alive = S:alive;
                |  N:alive = true; }
                |function g() : int { f(); return(23); }
                |initialiser scamp {
                |  f();
                |  int i = 1 + 35 * g();
                |}
                """.stripMargin)
    assert(messages.length === 2, "expecting two errors")
    assertMessage(messages, 0, 10, 22, "general function can't call a cell function")
    assertMessage(messages, 1, 12, 3, "initialiser can't call a cell function")
  }

  test("check access to state inside `cell` statement in initialiser") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state { boolean alive = false; }
                |updater {}
                |mapper { return(0); }
                |initialiser scamp {
                |  alive = S:alive;
                |  cell [50,50] {
                |    alive = S:alive;
                |    N:alive = true; }
                |  cell [10,20] N:alive = S:alive;
                |  N:alive = true; }
                """.stripMargin)
    assert(messages.length === 5, "expecting five errors")
    assertMessage(messages, 0, 8, 3, "initialiser can't access state outside of the body of a `cell` statement")
    assertMessage(messages, 1, 8, 11, "initialiser can't reference neighbour symbol outside of the body of a `cell` statement")
    assertMessage(messages, 2, 8, 13, "initialiser can't access state outside of the body of a `cell` statement")
    assertMessage(messages, 3, 13, 3, "initialiser can't reference neighbour symbol outside of the body of a `cell` statement")
    assertMessage(messages, 4, 13, 5, "initialiser can't access state outside of the body of a `cell` statement")
  }

  test("check indirect access to state inside `cell` statement in initialiser") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1], S = [0,-1];
                |state { boolean alive = false; }
                |updater {}
                |mapper { return(0); }
                |function f() {
                |  alive = S:alive;
                |  N:alive = true; }
                |function g() : int { f(); return(23); }
                |function h() : int { cell [0,0] f(); return(1); }
                |initialiser scamp {
                |  f();
                |  int temp = 25 + h() * 24;
                |  cell [50,50] {
                |    int temp2 = 10 + g();
                |    f(); }
                |  if temp <= 1 then g();
                |               else h(); }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 10, 22, "general function can't call a cell function")
    assertMessage(messages, 1, 11, 33, "initialiser function can't call a cell function")
    assertMessage(messages, 2, 13, 3, "initialiser can't call a cell function")
  }

  // Test missing `return` statement errors.

  test ("return statement in `then` block doesn't guarantee return value") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int alive = 0; }
                |updater {}
                |mapper { 
                |  if alive > 0 then return(0); }
                """.stripMargin)
    assert(messages.length === 1, "expecting a single error")
    assertMessage(messages, 0, 6, 1, "missing `return` in body of colour mapper declaration")
  }

  test ("return statements in both arms of an `if` statement guarantees return") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { int alive = 0; }
                |updater {}
                |mapper { 
                | if alive < 1
                |    then return(0);
                |    else {
                |      int temp = 10;
                |      return(temp); }}
                """.stripMargin)
    assert(messages.length === 0, "wasn't expecting any errors")
  }

  // Test inaccessible code errors

  test ("check detection of inaccessible code after returning statements") {
    val messages =
      semanticTestInline ("""
                |dimension (100,100);
                |neighbourhood N = [0,1];
                |state { boolean alive = false; }
                |updater {
                |  { return; int test = 0; test = test + 1; }
                |  alive = false; }
                |mapper { 
                |  return(0); 
                |  int test = 0;
                |  test = test + 1; }
                """.stripMargin)
    assert(messages.length === 3, "expecting three errors")
    assertMessage(messages, 0, 6, 13, "inaccessible statement (dead code)")
    assertMessage(messages, 1, 7, 3, "inaccessible statement (dead code)")
    assertMessage(messages, 2, 10, 3, "inaccessible statement (dead code)")
  }

}

