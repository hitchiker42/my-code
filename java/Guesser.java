// $Id: Guesser.java 127 2013-01-22 14:47:13Z cs671a $

package cs671;

/** A "guesser" interface.  A guesser is an object that produces a
 * series YES/NO questions and, based on replies to these questions,
 * eventually produces an answer to a secret finding problem.  The interface is
 * generic: type <code>T</code> represents the type of secrets
 * produced.  A guesser object is used by repeatedly calling
 * <code>makeQuestion</code> to produce a question and
 * <code>yes</code> or <code>no</code> to respond to the question,
 * until <code>hasSolved</code> returns <code>true</code>.  Before
 * this interaction starts, the guesser must be initialized using
 * method <code>initialize</code>.  All other methods throw
 * <code>IllegalStateException</code> if the guesser is not
 * initialized.
 *
 * @author  Michel Charpentier
 * @version 2.3, 01/22/13
 * @see HiLo
 * @see Liar
 * @param <T> the type of secrets
 */
public interface Guesser<T> {

  /** Initializes the guessing engine.  This method must be called to
   * initiate a problem before the rounds of questions and answers begin.
   *
   * @return An initialization message that can be displayed to the user
   */
  public String initialize ();

  /** Whether the problem has been solved.  While this method returns
   * <code>false</code>, users should continue to call methods
   * <code>makeQuestion</code>, <code>yes</code> and <code>no</code>.
   * Once it returns <code>true</code>, the method
   * <code>getSecret</code> can safely be called.
   *
   * @return <code>true</code> iff the problem has been solved
   * @throws IllegalStateException if the engine has not been initialized
   * @see #makeQuestion
   * @see #yes
   * @see #no
   * @see #getSecret
   */
  public boolean hasSolved ();

  /** The answer to the problem.  This method should be called after
   * <code>hasSolved</code> returns <code>true</code> to retreive the
   * solution to the problem.
   *
   * @return the answer to the problem, if it is known
   * @throws IllegalStateException if the problem has not yet been solved
   * or the guesser has not been initialized
   * @see #hasSolved
   */
  public T getSecret ();

  /** Used to reply YES to the previous question.  Method
   * <code>makeQuestion</code> must be called to generate a question
   * before calling <code>yes</code> or <code>no</code>.
   *
   * @throws IllegalStateException if <code>makeQuestion</code> has
   * not been called first or the guesser has not been initialized
   * @see #makeQuestion
   * @see #no
   */
  public void yes ();

  /** Used to reply NO to the previous question.  Method
   * <code>makeQuestion</code> must be called to generate a question
   * before calling <code>yes</code> or <code>no</code>.
   *
   * @throws IllegalStateException if <code>makeQuestion</code> has
   * not been called first or the guesser has not been initialized
   * @see #makeQuestion
   * @see #yes
   */
  public void no ();

  /** Generates a new question.  The previous question must be
   * answered (using <code>yes</code> or <code>no</code>) before a new
   * question is generated.
   *
   * @return the question, as a string to be displayed to the user
   * @throws IllegalStateException if the guesser is not initialized,
   * the problem is already solved or the previous question has not
   * been answered
   * @see #initialize
   * @see #hasSolved
   * @see #yes
   * @see #no
   */
  public String makeQuestion ();

  /** Indicates progress towards solving the problem.  After
   * initialization, the value returned is 0 (unless the problem is
   * immediately solved, in which case it is 1).  It always increases
   * as the guessing process progresses.  It is exactly 1 after the
   * problem is solved.
   *
   * @return a value between 0 and 1 that is a measure of how much
   * progress has been made towards solving the problem
   * @throws IllegalStateException if the engine has not been initialized
   */
  public double progress ();
}
