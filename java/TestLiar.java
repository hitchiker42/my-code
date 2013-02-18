// $Id: TestLiar.java 133 2013-02-07 16:27:03Z cs671a $

package tests;

import charpov.grader.*;
import static org.testng.Assert.*;

import cs671.Liar;
import java.util.Set;

@Test(val=50)
class TestLiar {

  Set<String> composers;
  Set<Number> numbers;

  void BEFORE () {
    composers = new java.util.TreeSet<String>();
    composers.add("Albeniz");
    composers.add("Borodin");
    composers.add("Chopin");
    composers.add("Debussy");
    composers.add("Enesco");
    composers.add("Franck");
    composers.add("Beethoven");
    composers.add("Berlioz");
    composers.add("Brahms");
    composers.add("Bruckner");
    numbers = new java.util.HashSet<Number>();
    for (int i=0; i<100; i++)
      numbers.add(Integer.valueOf(i));
  }

  @Test(val=1) void testFields1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    assertEquals(b.name, "composer");
  }

  @Test(val=1) void testFields2 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    assertEquals(b.maxLies, 5);
  }

  @Test(val=2) void testHasSolved1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    assertFalse(b.hasSolved());
  }

  @Test(val=2) void testHasSolved2 () {
    Set<Object> s = new java.util.HashSet<>(1);
    s.add(new Object());
    Liar<Object> b = new Liar<>(s, 5, "thing");
    b.initialize();
    assertTrue(b.hasSolved());
  }

  @Test(val=4) void testGetAnswer1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    while (!b.hasSolved()) {
      b.makeQuestion();
      b.yes();
    }
    Liar.Secret<String> s = b.getSecret();
    assertTrue(composers.contains(s.getSecret()));
    assertTrue(s.getLies() <= 5);
  }

  @Test(val=4) void testGetAnswer2 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    while (!b.hasSolved()) {
      if (b.makeQuestion().contains("Borodin"))
        b.yes();
      else
        b.no();
    }
    Liar.Secret<String> s = b.getSecret();
    assertEquals(s.getSecret(), "Borodin");
    assertEquals(s.getLies(), 0);
  }

  @Test(val=4) void testGetAnswer3 () {
    Set<Object> sec = new java.util.HashSet<>(1);
    sec.add("secret");
    Liar<Object> b = new Liar<>(sec, 5, "thing");
    b.initialize();
    Liar.Secret<Object> s = b.getSecret();
    assertTrue(b.hasSolved());
    assertEquals(s.getSecret(), "secret");
    assertEquals(s.getLies(), 0);
  }

  @Test(val=4) void testGetAnswer4 () {
    String cui = "Cui";
    composers.add(cui);
    Liar<String> b = new Liar<>(composers, 1, "composer");
    b.initialize();
    while (!b.hasSolved()) {
      if (b.makeQuestion().contains("Cui"))
        b.yes();
      else
        b.no();
    }
    Liar.Secret<String> s = b.getSecret();
    assertSame(s.getSecret(), cui);
    assertEquals(s.getLies(), 0);
  }

  @Test(val=4) void testSelectCandidates3 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.selectCandidates(1);
    b.initialize();
    Liar.Secret<String> s = b.getSecret();
    assertTrue(b.hasSolved());
    assertTrue(composers.contains(s.getSecret()));
    assertEquals(s.getLies(), 0);
  }

  @Test(val=4) void testGetAnswer7 () {
    getAnswer(new Liar<>(composers, 5, "composer"), "Borodin", .6);
  }

  @Test(val=4) void testGetAnswer5 () {
    getAnswer(new Liar<>(composers, 5, "composer"), "Borodin", .9);
  }

  @Test(val=4) void testGetAnswer6 () {
    getAnswer(new Liar<>(composers, 5, "composer"), "Borodin", .1);
  }

  <T> void getAnswer (Liar<T> b, T target, double d) {
    String t = String.valueOf(target);
    int lies = b.maxLies;
    b.initialize();
    while (!b.hasSolved()) {
      boolean lie = lies > 0 && Math.random() > d;
      if (lie)
        lies--;
      if (lie == b.makeQuestion().contains(t))
        b.no();
      else
        b.yes();
    }
    Liar.Secret<T> s = b.getSecret();
    assertEquals(s.getSecret(), target);
    assertEquals(s.getLies(), b.maxLies - lies);
  }

  @Test(val=4) void testGetAnswer8 () {
    Liar<Number> b = new Liar<>(numbers, 100, "number");
    b.initialize();
    while (!b.hasSolved()) {
      b.makeQuestion();
      b.no();
    }
    Liar.Secret<Number> s = b.getSecret();
    int n = s.getSecret().intValue();
    assertTrue(0 <= n && n < 100);
    assertTrue(s.getLies() <= 100);
  }

  @Test(val=4) void testGetAnswer9 () {
    getAnswer(new Liar<Number>(numbers, 100, "number"), Integer.valueOf(42), .6);
  }

  @Test(val=4) void testGetAnswer10 () {
    getAnswer(new Liar<Number>(numbers, 100, "number"), Integer.valueOf(42), .9);
  }

  @Test(val=4) void testGetAnswer11 () {
    getAnswer(new Liar<Number>(numbers, 100, "number"), Integer.valueOf(42), .1);
  }

  @Test(val=4) void testProgress1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    double d = b.progress();
    assertEquals(d, 0, 1e-5);
    assertTrue(d == 0, "close to 0 but not equal");
  }

  @Test(val=4) void testProgress2 () {
    Set<Object> s = new java.util.HashSet<>(1);
    s.add("secret");
    Liar<Object> b = new Liar<>(s, 5, "thing");
    b.initialize();
    double d = b.progress();
    assertEquals(d, 1, 1e-5);
    assertTrue(d == 1, "close to 1 but not equal");
  }

  @Test(val=2) void testProgress3 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    b.makeQuestion();
    b.yes();
    assertTrue(b.progress() > 0);
  }

  @Test(val=4) void testProgress4 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    while (!b.hasSolved()) {
      b.makeQuestion();
      b.yes();
    }
    double d = b.progress();
    assertEquals(d, 1, 1e-5);
    assertTrue(d == 1, "close to 1 but not equal");
  }

  @Test(val=4) void testProgress6 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    double p = 0;
    while (!b.hasSolved()) {
      b.makeQuestion();
      b.yes();
      assertTrue(p < (p = b.progress()), "progress has not increased");
    }
  }

  @Test(val=2) void testSelectCandidates1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.selectCandidates(1);
    b.initialize();
    assertTrue(b.hasSolved());
    assertEquals(b.getSecret().getLies(), 0);
  }

  @Test(val=2) void testSelectCandidates2 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    try {
      b.selectCandidates(0);
      fail("expected IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // OK
    }
  }

  @Test(val=2) void testState1 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    try {
      b.makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState2 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    try {
      b.getSecret();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState3 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    try {
      b.yes();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState4 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    try {
      b.selectCandidates(0);
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState5 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    b.makeQuestion();
    try {
      b.makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState6 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    try {
      b.progress();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState7 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    try {
      b.hasSolved();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState8 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    while (!b.hasSolved()) {
      b.makeQuestion();
      b.yes();
    }
    try {
      b.makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=2) void testState9 () {
    Liar<String> b = new Liar<>(composers, 5, "composer");
    b.initialize();
    b.makeQuestion();
    b.yes();
    try {
      b.no();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  // Big tests
  @Test(timeout=5000,val=2) void testBig1 () {
    testBig(242, 1000, 1000, .1);
  }

  @Test(timeout=5000,val=2) void testBig2 () {
    testBig(242, 1000, 1000, .6);
  }

  @Test(timeout=5000,val=2) void testBig3 () {
    testBig(242, 1000, 1000, .9);
  }

  @Test(timeout=10000,val=2) void testBig4 () {
    testBig(4242, 5000, 1000, .1);
  }

  @Test(timeout=10000,val=1) void testBig5 () {
    testBig(4242, 10000, 1000, .1);
  }

  @Test(timeout=30000,val=1) void testBig6 () {
    testBig(4242, 10000, 5000, .1);
  }

  @Test(timeout=1800000,val=1) void testBig7 () {
    testBig(14242, 20000, 10000, .1);
    }

  void testBig (int n, int s, int l, double d) {
    Integer target = Integer.valueOf(n);
    Set<Number> nums = new java.util.HashSet<>(s);
    for (int i=0; i<s; i++)
      nums.add(Integer.valueOf(i));
    Liar<Number> b = new Liar<>(nums, l, "number");
    b.initialize();
    getAnswer(b, target, d);
  }
}
