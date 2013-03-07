package cs671;
import java.util.*;

/** Simple wrapping on general-purpose timers.  This implementation
 * relies on {@code java.util.Timer} and adapts it to the
 * specification of {@code ClockTimer}.  Timers can be stopped and
 * restarted, <em>without switching to a new thread</em>.  The thread
 * is terminated when the timer is cancelled.
 *
 * <p>Instances of the class <em>are not thread-safe</em> (i.e., a
 * timer instance should not be shared among multiple threads).
 *
 * @author  Michel Charpentier
 * @version 3.1, 2/12/13
 * @see #cancel
 * @see java.util.Timer
 */
public class UtilClockTimer extends Timer implements ClockTimer {
  class ClockTask extends TimerTask{
    Runnable tock;
    long delay;
    ClockTask(){
      this.tock=null;
    }
    ClockTask(Runnable r){
      this.tock=r;
    }
    public void run(){
      throw new UnsupportedOperationException();
    }
    protected Object clone(){
      try{
        return super.clone();
      } catch (CloneNotSupportedException ex){
        return null;}
    }
  }
  ClockTask task;
  long delay;
  boolean state=false;
  /** Creates a new timer.  The timer is initially stopped.
   * @param r the timer task
   * @param d the timer delay, in milliseconds
   */
  public UtilClockTimer (Runnable r, long d) {
    this.delay=d;
    this.task=new ClockTask(r);
  }

  /** Creates a new timer.  The timer has no task and no delay. */
  public UtilClockTimer () {
    this.delay=0;
    this.task=new ClockTask();
  }

  public boolean isRunning () {
    return state;
  }

  public Runnable setRunnable (Runnable r) {
    task.tock=r;
    return r;
  }

  public void setDelay (long d) {
    delay=d;
  }

  public void start () {
    state=true;
    schedule(task,task.delay);
  }
  @SuppressWarnings("unchecked")
  public void stop () {
    state=false;
    ClockTask temp=(ClockTask)task.clone();
    task.cancel();
    task=temp;
  }

  public void cancel () {
    task.cancel();
    }
}