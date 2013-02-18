// $Id: TestGuesserTextUI.java 133 2013-02-07 16:27:03Z cs671a $

package tests;

import charpov.grader.*;
import static org.testng.Assert.*;

import cs671.GuesserTextUI;
import java.util.Set;
import charpov.util.Answer;
import charpov.util.TailWriter;

@Test(val=30)
class TestGuesserTextUI {

  TailWriter w;

  void BEFORE () {
    w = new TailWriter(4096);
  }

  void AFTER () {
    w.close();
  }

  @Test(val=6) void testUI1 () {
    int n = new GuesserTextUI(new SillyGuesser(10), new Answer("ny"), w).play();
    assertEquals(n, 1);
    assertTrue(w.getTail().contains("YES:5 NO:5"));
  }

  @Test(val=6) void testUI6 () {
    int n = new GuesserTextUI(new SillyGuesser(10), new Answer("newnyty"), w).play();
    assertEquals(n, 2);
    assertTrue(w.getTail().contains("YES:5 NO:5"));
  }

  @Test(timeout=20000,val=4) void testUI3 () {
    int n = new GuesserTextUI(new SillyGuesser(1000000), new Answer("ynn"), w).play();
    assertEquals(n, 1);
    assertTrue(w.getTail().contains("YES:333334 NO:666666"));
  }

  @Test(timeout=20000,val=4) void testUI4 () {
    StringBuilder b = new StringBuilder();
    for (int i=1; i<1000; i++)
      b.append("y");
    b.append("n");
    int n
      = new GuesserTextUI(new SillyGuesser(1000), new Answer(b.toString()), w).play();
    assertEquals(n, 1000);
    assertTrue(w.getTail().contains("YES:999 NO:1"));
  }

  @Test(val=4) void testUI2 () {
    int n = new GuesserTextUI(new SillyGuesser(10), new Answer("yn"), w).play();
    assertEquals(n, 2);
    assertTrue(w.getTail().contains("YES:5 NO:5"));
  }

  @Test(val=2) void testUI5 () {
    int n = new GuesserTextUI(new SillyGuesser(0), new Answer("n"), w).play();
    assertEquals(n, 1);
    assertTrue(w.getTail().contains("YES:0 NO:0"));
  }
}
