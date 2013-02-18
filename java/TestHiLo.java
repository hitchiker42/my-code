// $Id: TestHiLo.java 133 2013-02-07 16:27:03Z cs671a $

package tests;

import charpov.grader.*;
import static org.testng.Assert.*;

import cs671.HiLo;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

@Test(val=20)
class TestHiLo {

  @Test(val=2) void consParam1 () {
    try {
      new HiLo(5, 4);
      fail("expected IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // OK
    }
  }

  @Test(val=2) void consParam2 () {
    new HiLo(3, 3);
    new HiLo(-5, 0);
    new HiLo(Integer.MIN_VALUE, Integer.MAX_VALUE);
  }

  @Test(val=1) void state1 () {
    try {
      new HiLo(1,10).makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state2 () {
    HiLo hilo = new HiLo(1,10);
    hilo.initialize();
    try {
      hilo.yes();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state3 () {
    HiLo hilo = new HiLo(1,10);
    hilo.initialize();
    hilo.makeQuestion();
    try {
      hilo.makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state4 () {
    HiLo hilo = new HiLo(1,10);
    hilo.initialize();
    hilo.makeQuestion();
    hilo.yes();
    try {
      hilo.no();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state5 () {
    HiLo hilo = new HiLo(1,10);
    try {
      hilo.getSecret();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state6 () {
    HiLo hilo = new HiLo(1,10);
    try {
      hilo.progress();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state7 () {
    HiLo hilo = new HiLo(1,10);
    try {
      hilo.hasSolved();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state8 () {
    HiLo hilo = new HiLo(1,10);
    hilo.initialize();
    while (!hilo.hasSolved()) {
      hilo.makeQuestion();
      hilo.yes();
    }
    try {
      hilo.makeQuestion();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=1) void state9 () {
    HiLo hilo = new HiLo(1,10);
    try {
      hilo.yes();
      fail("expected IllegalStateException");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test(val=4) void getSecret1 () throws Exception {
    getSecret(new HiLo(0,999), 421);
  }

  @Test(val=4) void getSecret2 () throws Exception {
    getSecret(new HiLo(0,999), 0);
  }

  @Test(val=4) void getSecret3 () throws Exception {
    getSecret(new HiLo(0,999), 999);
  }

  @Test(val=4) void getSecret4 () throws Exception {
    getSecret(new HiLo(0,2000000000), 123456789);
  }

  @Test(val=2) void getSecret5 () throws Exception {
    getSecret(new HiLo(Integer.MIN_VALUE,Integer.MAX_VALUE), 0);
  }

  @Test(val=2) void getSecret6 () throws Exception {
    HiLo hilo = new HiLo(42, 42);
    hilo.initialize();
    assertTrue(hilo.hasSolved());
    assertEquals(hilo.getSecret().intValue(), 42);
  }

  static Pattern num = Pattern.compile("-?\\d+");
  static Pattern larger =
    Pattern.compile("(?i:larger)|(?i:higher)|(?i:more)|(?i:greater)");
  static Pattern smaller =
    Pattern.compile("(?i:smaller)|(?i:lower)|(?i:less)");

  void getSecret (HiLo hilo, int target) throws NumberFormatException {
    hilo.initialize();
    while (!hilo.hasSolved()) {
      String q = hilo.makeQuestion();
      Matcher m = num.matcher(q);
      if (!m.find())
        fail("no number in question");
      boolean ans = false;
      int n = Integer.parseInt(m.group());
      if (larger.matcher(q).find())
        ans = target > n;
      else if (smaller.matcher(q).find())
        ans = target < n;
      else
        fail("malformed question");
      if (ans)
        hilo.yes();
      else
        hilo.no();
    }
    assertEquals(hilo.getSecret().intValue(), target);
  }

  @Test(val=2) void progress1 () {
    HiLo hilo = new HiLo(1,1000);
    hilo.initialize();
    double d = hilo.progress();
    assertEquals(d, 0, 1e-5);
    assertTrue(d == 0, "close to 0 but not equal");
  }

  @Test(val=1) void progress4 () {
    HiLo hilo = new HiLo(100,100);
    hilo.initialize();
    double d = hilo.progress();
    assertEquals(d, 1, 1e-5);
    assertTrue(d == 1, "close to 1 but not equal");
  }

  @Test(val=2) void progress2 () {
    HiLo hilo = new HiLo(1,1000);
    hilo.initialize();
    while (!hilo.hasSolved()) {
      hilo.makeQuestion();
      hilo.yes();
    }
    double d = hilo.progress();
    assertEquals(d, 1, 1e-5);
    assertTrue(d == 1, "close to 1 but not equal");
  }

  @Test(val=2) void progress3 () {
    HiLo hilo = new HiLo(1,1000);
    hilo.initialize();
    double d = 0;
    while (!hilo.hasSolved()) {
      hilo.makeQuestion();
      hilo.yes();
      assertTrue(d < (d = hilo.progress()), "progress() has not increased");
    }
  }
}
