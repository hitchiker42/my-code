// $Id: SampleTests1.java 133 2013-02-07 16:27:03Z cs671a $

package tests;

import charpov.grader.*;
import static org.testng.Assert.*;

import cs671.HiLo;
import cs671.Liar;
import cs671.GuesserTextUI;
import cs671.Guesser;

import java.util.Set;
import charpov.util.Answer;
import charpov.util.TailWriter;

class SampleTests1 {

  public static void main (String[] args) throws Exception {
    java.util.logging.Logger.getLogger("charpov.grader")
      .setLevel(java.util.logging.Level.WARNING);
    new Tester(SampleTests1.class,
               TestHiLo.class,
               TestLiar.class,
               TestGuesserTextUI.class).run();
  }

  @Test void sample1 () {
    new HiLo(3, 3);
    new HiLo(-5, 0);
    new HiLo(Integer.MIN_VALUE, Integer.MAX_VALUE);
  }

  @Test void sample2 () {
    HiLo hilo = new HiLo(0, 999);
    try {
      hilo.makeQuestion();
      fail("IllegalStateException expected");
    } catch (IllegalStateException e) {
      // OK
    }
  }

  @Test void sample3 () {
    HiLo hilo = new HiLo(0, 999);
    hilo.initialize();
    while (!hilo.hasSolved()) {
      hilo.makeQuestion();
      hilo.yes();
    }
    assertTrue(hilo.progress() == 1);
  }

  @Test void sample4 () {
    Set<Integer> numbers = new java.util.HashSet<>();
    numbers.add(Integer.valueOf(2013));
    Liar<Integer> b = new Liar<>(numbers, 5, "number");
    assertEquals(b.name, "number");
  }

  @Test void sample5 () {
    Set<String> composers = new java.util.TreeSet<>();
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

  @Test void sample6 () {
    TailWriter w = new TailWriter(4096);
    Guesser<String> g = new SillyGuesser(10);
    // a fake user is answering: n, e, w, n, y, t, y, n, e, w, n, y, t, y, n, ...
    int n = new GuesserTextUI(g, new Answer("newnyty"), w).play();
    assertEquals(n, 2);
    assertTrue(w.getTail().contains("YES:5 NO:5"));
    w.close();
  }
}

/** A guesser that asks an exact number of questions and counts the
 * number of yes and no answers.
 */
class SillyGuesser implements Guesser<String> {

  private int yes, no;
  private final int count;
  private boolean q;

  public SillyGuesser (int n) {
    count = n;
    yes = -1;
  }

  public String initialize () {
    yes = no = 0;
    q = true;
    return "Let's go!";
  }

  public boolean hasSolved () {
    if (yes < 0)
      throw new IllegalStateException();
    return yes + no == count;
  }

  public String getSecret () {
    if (yes < 0 || !hasSolved())
      throw new IllegalStateException();
    return "YES:" + yes + " NO:" + no;
  }

  public void yes () {
    if (yes < 0 || q)
      throw new IllegalStateException();
    yes++;
    q = true;
  }

  public void no () {
    if (yes < 0 || q)
      throw new IllegalStateException();
    no++;
    q = true;
  }

  public String makeQuestion () {
    if (yes < 0 || hasSolved() || !q)
      throw new IllegalStateException();
    q = false;
    return "yes or no?";
  }

  public double progress () {
    if (yes < 0)
      throw new IllegalStateException();
    if (hasSolved())
      return 1;
    return (double)(yes + no) / count;
  }
}