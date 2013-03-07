package cs671;
import java.util.*;
import static java.util.Collections.*;
/** Simple timers.  This implementation does not rely on more general
 * timers.  Each instance of {@code SimpleClockTimer} has its own
 * thread.  Timers can be stopped and restarted, <em>without switching
 * to a new thread</em>.  The thread is terminated when the timer is
 * cancelled.
 *
 * <p> Instances of this class <em>are thread-safe</em> (i.e., a timer
 * instance can be shared among multiple threads).
 *
 * @author  Michel Charpentier
 * @version 3.1, 2/12/13
 * @see #cancel
 */
public class SimpleClockTimer implements ClockTimer {
  /*class ClockTask{
    Thread tick;
    Runnable tock;
    long delay;
    boolean alive=false;
    ClockTask(){
      this.tick=new Thread((Runnable)null);
      this.tock=null;
      this.delay=0;
      this.alive=true;
    }
    ClockTask(Runnable r,long delay){
      this.tock=r;
      this.delay=delay;
      this.tick=new Thread(this.tock);
      this.alive=true;
    }
    public void run(){

      if (alive){
        try{
          Thread.sleep(delay);
          tick.run();
        } catch (InterruptedException ex ){}
      }
    }
    void stop(){tick.interrupt();}
    void cancel(){alive=false;
        tick.interrupt();}
  }*/
  Runnable task;
  Thread thread;
  long delay;
  boolean state;
  boolean alive=true;
  boolean started=false;
  /** Creates a new timer.  The timer is initially stopped.
   * @param r the timer task
   * @param d the timer delay, in milliseconds
   */
  public SimpleClockTimer (Runnable r, long d) {
    this.task=r;
    this.thread=new Thread(this.task);
    this.delay=d;
    this.state=false;
  }

  /** Creates a new timer.  The timer has no task and no delay. */
  public SimpleClockTimer () {
    this.task=null;
    this.thread=new Thread(this.task);
    this.delay=0;
    this.state=false;
  }

  public boolean isRunning () {
    return state;
  }

  public Runnable setRunnable (Runnable r) {
    task=r;
    return task;
  }

  public void setDelay (long d) {
    delay=d;
  }
  boolean startable;
  public void start () {
    if(alive){
      startable=true;
      try{
        Thread.sleep(delay);
      } catch (InterruptedException ex){}
      if(startable){
        state=true;
        task.run();
      }
    }
  }
  public void stop () {
    startable=false;
    state=false;
  }

  public void cancel () {
    startable=false;
    state=false;
    alive=false;
  }
}