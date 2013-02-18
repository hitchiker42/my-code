// $Id: Test.java 133 2013-02-07 16:27:03Z cs671a $

package cs671;

import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.lang.annotation.Retention;

/** Test annotation.  This is the only annotation used in the testing system.
 *
 * @author  Michel Charpentier
 * @version 1.1, 02/01/13
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Test {

  /** The weight of this test.  Tests with negative weight are silently
   * ignored. */
  double weight () default 1;

  /** A description of the test. */
  String info () default "";
}