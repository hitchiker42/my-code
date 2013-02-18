// $Id: SampleTests2.java 134 2013-02-13 20:34:11Z cs671a $

package tests;

import static org.testng.Assert.*;

import cs671.*;

import java.util.*;
import java.lang.reflect.Method;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.PrintStream;
import java.io.ByteArrayOutputStream;

class SampleTests2 {

  public static void main (String[] args) throws Exception {
    java.util.logging.Logger.getLogger("charpov.grader")
      .setLevel(java.util.logging.Level.WARNING);
    charpov.grader.Tester tester = new charpov.grader.Tester(Sample1.class);
    tester.run();
  }
}

class Sample1 {

  TestResult[] results;
  String[] output;

  Sample1 () {
    System.out.println("Initializing tests...");
    Tester tester = new Tester(SomeTests.class);
    StringWriter w = new StringWriter();
    PrintWriter out = new PrintWriter(w);
    tester.setPrintWriter(out);
    tester.run();
    out.close();
    results = tester.getResults().toArray(new TestResult[4]);
    output = w.toString().split("\n");
  }

  @charpov.grader.Test(val=1) void testCount () {
    assertEquals(results.length, 4);
  }

  @charpov.grader.Test(val=1) void testSuccess () {
    assertFalse(results[0].success());
    assertTrue(results[1].success());
    assertTrue(results[2].success());
    assertFalse(results[3].success());
  }

  @charpov.grader.Test(val=1) void testWeight () {
    assertEquals(results[0].getWeight(), 2, 1e-5);
    assertEquals(results[1].getWeight(), 3, 1e-5);
    assertEquals(results[2].getWeight(), 1, 1e-5);
    assertEquals(results[3].getWeight(), 1, 1e-5);
  }

  @charpov.grader.Test(val=1) void testInfo () {
    assertEquals(results[0].getInfo(),
                 "tests.SomeTests.test1: a test that 2+2=5");
    assertEquals(results[1].getInfo(),
                 "tests.SomeTests.test2: a test that 2+2=4");
    assertEquals(results[2].getInfo(),
                 "tests.SomeTests.test3: a long running test");
    assertEquals(results[3].getInfo(),
                 "tests.SomeTests.test4");
  }

  @charpov.grader.Test(val=1) void testCause () {
    assertNull(results[1].error());
    assertNull(results[2].error());
    assertTrue(results[0].error() instanceof AssertionError);
    assertTrue(results[3].error() instanceof OutOfMemoryError);
  }

  @charpov.grader.Test(val=1) void testDuration () {
    assertTrue(results[0].getDuration() < .1);
    assertTrue(results[1].getDuration() < .1);
    double d = results[2].getDuration();
    assertTrue(d > 5 && d < 6);
  }

  @charpov.grader.Test(val=1) void testWarnings () {
    assertEquals(output.length, 3);
    assertTrue(output[0].contains("failTest"));
    assertTrue(output[1].contains("initTest3"));
    assertTrue(output[2].contains("test6"));
    assertTrue(output[2].contains("static"));
  }

  @charpov.grader.Test(val=1) void testEx () {
    class C extends TestableAdapter {
    }
    Tester tester = new Tester(C.class);
    tester.setPrintWriter(null);
    tester.run();
  }

  @charpov.grader.Test(val=1) void testT1 () {
    Tester tester = new Tester(T.class);
    tester.run();
    assertEquals(tester.getResults().size(), 3);
    TestResult r = tester.getResults().get(0);
    assertFalse(r.success());
    assertTrue(r.error() instanceof T.E);
    assertEquals(r.getInfo(), "tests.T.bar: FAIL");
    r = tester.getResults().get(1);
    assertTrue(r.success());
    assertNull(r.error());
    assertEquals(r.getInfo(), "tests.T.foo: PASS");
    r = tester.getResults().get(2);
    assertFalse(r.success());
    assertTrue(r.error() instanceof T.E);
    assertEquals(r.getInfo(), "tests.T.foobar");
  }

  @charpov.grader.Test(val=1) void testT2 () {
    PrintStream out = System.out;
    PrintStream err = System.err;
    try {
      ByteArrayOutputStream w = new ByteArrayOutputStream();
      PrintStream p = new PrintStream(w);
      System.setOut(p);
      System.setErr(p);
      Tester.main(new String[] {"tests.T"});
      p.close();
      String[] lines = w.toString().split("\n");
      assertEquals(lines.length, 6);
      assertEquals(lines[0].trim(), "SUCCESSFUL TESTS:");
      assertTrue(lines[1].startsWith("  tests.T.foo: PASS (2.3) in "));
      assertTrue(lines[1].endsWith(" milliseconds"));
      assertEquals(lines[2].trim(), "FAILED TESTS:");
      assertEquals(lines[3],
                   "  tests.T.bar: FAIL (7.0) from tests.T$E");
      assertEquals(lines[4],
                   "  tests.T.foobar (0.7) from tests.T$E");
      assertTrue(lines[5].contains("SCORE"));
      assertTrue(lines[5].contains("23"));
    } finally {
      System.setOut(out);
      System.setErr(err);
    }
  }
}

class T extends TestableAdapter {
  @SuppressWarnings("serial")
  static class E extends Throwable {
  }
  @Test(weight=2.3, info="PASS") private void foo () {
  }
  @Test(weight=7, info="FAIL") int bar () throws Throwable {
    throw new E();
  }
  @Test(weight=.7) int foobar () throws Throwable {
    throw new E();
  }
}

class TestableAdapter implements Testable {
  public boolean beforeMethod (Method m) throws Exception {
    return true;
  }
  public void afterMethod (Method m) throws Exception {
  }
}

class SomeTests extends TestableAdapter {

  public boolean beforeMethod (Method m) throws Exception {
    String name = m.getName();
    System.out.println(name);
    if (name.startsWith("init"))
      return false;
    if (name.startsWith("fail"))
      throw new Exception("failed to init");
    Thread.sleep(1000);
    return true;
  }

  public void afterMethod (Method m) throws InterruptedException {
    Thread.sleep(1000);
  }

  @Test(weight=Math.PI)
  void failTest () {
  }

  @Test(weight=2, info="a test that 2+2=5")
  void test1 () {
    assertEquals(2 + 2, 5);
  }

  @Test(weight=3, info="a test that 2+2=4")
  int test2 () {
    assertEquals(2 + 2, 4);
    return 42;
  }

  @Test void initTest3 () {
  }

  @Test(info="a long running test")
  void test3 () throws InterruptedException {
    Thread.sleep(5432);
  }

  @Test void test4 () {
    throw new OutOfMemoryError();
  }

  @Test(weight=-1)
  void test5 () {
    System.out.println("not run for now");
  }

  @Test static void test6 () {
    System.out.println("never run");
  }
}