package cs671;
import static cs671.Debug.*;
import java.util.*;
import static java.util.Collection.*;
import static java.util.Arrays.*;
import java.util.concurrent.locks.*;

/** Binary clocks.  These clocks can either be passive objects or
 * include their own timer.  In the latter case, their value is
 * updated every second.  Clock updates (automatic or manual) may
 * increase or decrease the clock value depending on its current
 * "direction".  The direction of a clock can be changed at any time.
 * It is {@link Direction#FORWARD} by default.
 *
 * <p> The index of the least significant bit is 0.  In particular,
 * this means that the value of <code>getBit(0)</code> (or,
 * equivalently, the value of <code>getValues()[0]</code> changes with
 * each call to <code>step()</code> and, if the clock is running, with
 * each tick.
 *
 *<p> Clocks are observable and every state change is forwarded to the
 * clock's observers.  Note that some method calls do not trigger an
 * update, for instance if a clock is set to a value equal to its
 * current value or if the clock's direction is set to a value equal
 * to its current direction.
 *
 *<p> Since a timer thread (on active clocks) needs to access the
 * state of the clock, all state-changing and state-querying methods
 * are thread-safe.
 *
 * @author  Michel Charpentier
 * @version 3.1, 2/11/13
 * @see ClockTimer
 * @see java.util.Observable
 */
@SuppressWarnings("unchecked")
public class Clock extends java.util.Observable {
  static final String DEFAULT_TIMER_CLASS = SimpleClockTimer.class.getName();
  static long time(int scale){
    return scale*(System.currentTimeMillis()/1000L);
  }
  static long time(){
    return System.currentTimeMillis();
  }
  class ClockRunner implements Runnable{
    long time;
    ClockRunner(){
      this.time=Clock.time();
    }
    public void run(){
      while(true){
        for (int i=0;i<((Clock.time()-time)/1000);i++){
          step();
        }
      }
    }
  }
  /**
   *The actual binary data of the clock
   */
  ClockTimer timer;
  private BitSet clock;
  private BitSet temp_clock;
  private final BitSet _true;
  private final BitSet _false;
  private final int size;
  private final int words;
  private boolean temp;
  private Direction direction=Direction.FORWARD;
  private void update(){
      BitSet b=((BitSet)temp_clock.clone());
      b.xor(clock);
      notifyObservers(b);
    }
  private void before(){
      temp_clock=(BitSet)clock.clone();
  }
  private void after(){
    if(!(clock.equals(temp_clock))){
      update();
    }
  }
  /** The "lock" that guards all clock state changes.  Every state
   * change, including automatic changes on active clocks, is
   * performed while owning this lock.
   */
  private final Lock real_lock=new ReentrantLock();
  protected final Object lock=real_lock;
  //should timer get its own lock?
  //Can this V work?
  /*synchronized (lock) boolean update(Immutable_BitSet test){
    if (test!=public_clock){
    return false;
    } else {
    pubilc_clock=clock;
    return true;
    }
    }*/
  /** Constructs a passive clock with <code>nbBits</code> bits.  Initially, all
   * bits are off (false).  The clock has no timer.
   *
   * @param nbBits the number of bits of this clock
   * @throws IllegalArgumentException if <code>nbBits &lt; 1</code>
   */
  public Clock (int nbBits) {
    if(nbBits<=0){
      throw new IllegalArgumentException();
    }
    this.clock=new BitSet(nbBits);
    this.temp_clock=(BitSet)this.clock.clone();
    (this._true=new BitSet(nbBits)).set(0,nbBits);
    (this._false=new BitSet(nbBits)).clear(0,nbBits);
    this.size=nbBits;
    this.words=this.size/64+1;
  }

  /** Constructs an active clock with <code>nbBits</code> bits.  Initially,
   * all bits are off (false) and the clock is associated with a new
   * timer of the specified class, if one can be constructed using a
   * no-argument constructor.  The clock is initially stopped.
   *
   * @param nbBits the number of bits of this clock
   * @param timerClass the name of a timer-implementing class.  The
   * class must implement the {@code ClockTimer} interface and it must
   * have a public, no-argument constructor.  Clock implementations
   * <em>must</em> at least accept {@code "cs671.SimpleClockTimer"} and
   * {@code "cs671.UtilClockTimer"} as valid timer classes.
   * @throws IllegalArgumentException if <code>nbBits &lt; 1</code> or if
   * the specified class cannot be loaded, cannot be instantiated or
   * is not of type {@code ClockTimer}
   * @see ClockTimer
   */
  public Clock (int nbBits, String timerClass) { // bonus question
    if(nbBits<=0){
      throw new IllegalArgumentException();
    }
    this.clock=new BitSet(nbBits);
    this.temp_clock=(BitSet)this.clock.clone();
    (this._true=new BitSet(nbBits)).set(0,nbBits);
    (this._false=new BitSet(nbBits)).clear(0,nbBits);
    this.size=nbBits;
    this.words=(this.size/64)+1;
    /*case class in
      UtilClockTimer) ;;
      SimpleClockTimer) ;;
      Default) try to get class timerClass,
      assert timerClass implements ClockTimer
      try to instantiate timerClass
    */
    throw new UnsupportedOperationException();
  }

  /** Constructs an active clock with <code>nbBits</code> bits.  Initially,
   * all bits are off (false) and the clock is associated with the given
   * timer.  The clock is initially stopped.
   *
   * @param nbBits the number of bits of this clock
   * @param t a timer; if the timer already has a delay and a
   * runnable, they will be reset
   * @throws IllegalArgumentException if <code>nbBits &lt; 1</code> or
   * if timer {@code t} is running
   */
  public Clock (int nbBits, ClockTimer t) {
    if(nbBits<=0){
      throw new IllegalArgumentException();
    }
    this.clock=new BitSet(nbBits);
    this.temp_clock=(BitSet)this.clock.clone();
    this.size=nbBits;
    this.words=(this.size/64)+1;
    (this._true=new BitSet(nbBits)).set(0,nbBits);
    (this._false=new BitSet(nbBits)).clear(0,nbBits);
    this.timer=t;
    this.timer.setRunnable(new ClockRunner());
    this.timer.setDelay(0);
  }

  /** Clock size
   * @return the number of bits in the clock
   */
  public int size () {
    return size;
  }

  /** Permanently terminates the timer of this clock.  A terminated
   * clock cannot be restarted and its timer becomes garbage.  The
   * clock is now a passive object.  The state of a passive clock can
   * still be changed with the various "set" methods but won't change
   * on its own.  If the clock was already passive, the method has no
   * effect and observers are not notified.
   *
   * @see ClockTimer#cancel
   */
  public void destroy () {
    real_lock.lock();
    try{
      timer.cancel();
    } catch(NullPointerException ex){}
      finally{
      real_lock.unlock();
    }
  }

  /** Starts the clock.  The first bit update occurs after 1
   * second and every second after that, until the clock is stopped.
   *
   * @throws IllegalStateException if the clock is passive or is already running
   */
  public void start () {
    if (timer==null){
      throw new IllegalStateException();
    }
    real_lock.lock();
    try{
      timer.start();
    } finally{
      real_lock.unlock();
    }
  }

  /** Stops the clock.  Bit updates stop occurring immediately.  The
   * clock is <em>not</em> passive; it can be restarted later.
   *
   * @throws IllegalStateException if the clock is passive or is not running
   */
  public void stop () {
    real_lock.lock();
    try{
      timer.stop();
    } finally{
      real_lock.unlock();
    }
  }

  /** The status of the clock, as a boolean.
   *
   * @return true iff the clock is currently running.
   */
  public boolean isTicking () {
    return timer.isRunning();
  }

  /** Resets the clock.  All bits are set to zero.  If the clock is
   * running, the next bit update will happen 1 second after bits are
   * cleared.  Observers are notified if the clock is running or if it
   * was non-zero.
   */
  public void clear () {
    real_lock.lock();
    try{
      before();
      clock=(BitSet)_false.clone();
      after();
    } finally{
      real_lock.unlock();
    }
  }

  /** The value of bit number <code>n</code>.  Least significant bit is bit
   * number 0; most significat bit is bit <code>size()-1</code>.
   *
   * @param n bit number
   * @return boolean value of that bit.
   * @throws IndexOutOfBoundsException if no such bit exists
   */
  public boolean getBit (int n) {
    if(n<0 || n>=size){
      throw new IndexOutOfBoundsException();
    }
    real_lock.lock();
    try{
      return clock.get(n);
    } finally{
      real_lock.unlock();
    }
  }

  /** Sets bit number <code>n</code> to true.  Least significant bit is bit
   * number 0; most significat bit is bit <code>size()-1</code>.
   *
   * @param n bit number to be set to true
   * @return boolean value of that bit before it is set.
   * @throws IndexOutOfBoundsException if no such bit exists
   */
  public boolean setBit (int n) {
    if(n<0 || n>=size){
      throw new IndexOutOfBoundsException();
    }
    real_lock.lock();
    try{
      before();
      temp=clock.get(n);
      clock.set(n);
      after();
    } finally{
      real_lock.unlock();
    }
    return temp;
  }
/** Sets bit number <code>n</code> to false.  Least significant bit is bit
 * number 0; most significat bit is bit <code>size()-1</code>.
 *
 * @param n bit number to be set to false
 * @return boolean value of that bit before it is set.
 * @throws IndexOutOfBoundsException if no such bit exists
 */
  public boolean clearBit (int n) {
    if(n<0 || n>=size){
      throw new IndexOutOfBoundsException();
    }
    real_lock.lock();
    try{
      before();
      temp=clock.get(n);
      clock.clear(n);
      after();
    } finally{
      real_lock.unlock();
    }
    return temp;
  }
/** Sets bit number <code>n</code> to its next value.  If the bit
 * was false, it is now true; if it was true, it is now false.  The
 * method returns a "carry" (true when the bit changes from true to
 * false).  Roughly speaking, this is a <code>+1</code> operation on
 * the given bit.  Since the state of the clock is guaranteed to
 * change, observers are always notified.
 *
 * @param n bit number to be set to next value
 * @return carry after the bit is set.
 * @throws IndexOutOfBoundsException if no such bit exists
 */
  public boolean nextBit (int n) {
    if(n<0 || n>=size){
      throw new IndexOutOfBoundsException();
    }
    real_lock.lock();
    try{
      before();
      temp=clock.get(n);
      clock.flip(n);
      after();
    } finally{
      real_lock.unlock();
    }
    return temp;
  }
/** Clock direction: FORWARD or BACKWARD.
 * @see #setDirection
 */
  public enum Direction {
    /** Forward direction.  Forward steps increase the binary value of
     * the clock by one.  "111111" becomes "000000".
     */
    FORWARD {
      public Direction reverse () {
        throw new UnsupportedOperationException();
      }
    },
    /** Backward direction.  Backward steps decrease the binary value of
     * the clock by one.  "000000" becomes "111111".
     */
    BACKWARD {
      public Direction reverse () {
        throw new UnsupportedOperationException();
      }
    };
    /** Reverses the direction.
     * @return the other direction, i.e., {@code FORWARD.reverse()}
     * returns {@code BACKWARD} and vice-versa.
     */
    abstract public Direction reverse ();
  }
  /** Sets the clock direction, FORWARD or BACKWARD. */
  public void setDirection (Direction d) {
    direction=d;
  }
  /** Gets the clock direction.
   * @return the clock's current direction
   */
  public Direction getDirection () {
    return direction;
  }
  /** Steps the clock.  This method increases or decreases the value
   * of the clock by one, according to the current direction.  Note
   * that bit number 0 is guaranteed to change as a result of calling
   * this method and therefore observers are always notified.
   *
   * @see #setDirection
   */
  public void step () {
    real_lock.lock();
    try{
      before();
      if (direction==Direction.BACKWARD){
        clock.xor(_true);
      }
      for (int i=0;i<size;i++){
        if(clock.get(i)){
          clock.flip(i);
        } else {
          clock.flip(i);
          break;
        }
      }
      if (direction==Direction.BACKWARD){
        clock.xor(_true);
      }
      after();
    } finally{
      real_lock.unlock();
    }
  }

/** Sets each bit value according to the array of booleans.  The
 * array size <em>must</em> equal the number of bits in the clock.
 * Bit number <code>i</code> in the clock is set to the value of
 * boolean number <code>i</code> in the array (i.e., the clock and
 * the array store bits in the same direction).  Note that observers
 * are notified only if parameter {@code v} and the clock differ by
 * at least one bit.
 *
 * Likely rather inefficent compared to setLongValue
 * @param v boolean value for each bit
 * @throws IllegalArgumentException if the size of the array is
 * different from the number of bits in the clock.
 */
  public void setValue (boolean[] v) {
    if(v.length!=size){
      throw new IllegalArgumentException();
    }
    real_lock.lock();
    try{
      before();
      for (int i=0;i<v.length;i++){
        if(v[i]){clock.set(i);}else{clock.clear(i);}
      }
      after();
    } finally{
      real_lock.unlock();
    }
  }

/** Sets each bit value according the long parameter.  If the clock
 * has more than 64 bits, bits beyond 63 are set to zero.  The least
 * significant bit of the long is also the least significant bit of
 * the clock, e.g., after <code>setLongValue(1L)</code>,
 * <code>getBit(0)</code> is true.  Note that observers
 * are notified only if parameter {@code v} and the clock differ by
 * at least one bit.
 *
 * @param v value for each bit of the clock
 * @throws IndexOutOfBoundsException if <code>value</value> has a
 * bit set to true beyond the clock's capacity.
 */
  public void setLongValue (long v) {
    if (size<63){
      if(v>=Math.pow(2,size-1)||v<-Math.pow(2,size-1)){
        throw new IndexOutOfBoundsException();
      }
    }
    long[] temp_long={v};
  real_lock.lock();
  try{
    before();
    clock=BitSet.valueOf(temp_long);
    after();
  } finally{
    real_lock.unlock();
  }
  }
/** Boolean value for each bit, as an array.  Modifications to this
 * array do not change the clock value.  Boolean number
 * <code>i</code> in the array is equal to bit number <code>i</code>
 * in the clock (i.e., the clock and the array store bits in the same
 * direction).
 *
 * @return boolean value for each bit
 */
  public boolean[] getValue () {
    real_lock.lock();
    boolean[] val=new boolean[size];
    byte[] bytes;
    try{
      bytes=clock.toByteArray();
    } finally{
      real_lock.unlock();
    }
    for(int i=0;i<size;i++){
      val[i]=((bytes[i/8] & (1<<(i%8))) != 0);
    }
    return val;
  }
/** All bit values, as a long.  The least
 * significant bit of the long is also the least significant bit of
 * the clock.
 * @return boolean value for each bit
 * @throws IllegalStateException ff the clock has more
 * than 64 bits <em>and</em> at least one bit beyond 63 is set
 */
  public long getLongValue () {
    long temp_long=0;
    real_lock.lock();
    try{
      if (!(size<=0) && size<63 || clock.get(64,size).isEmpty()){
        here(toString(clock.toLongArray()));
        here((clock.toLongArray().length));
        temp_long=clock.toLongArray()[0];
        return temp_long;
      } else {
        throw new IndexOutOfBoundsException();
      }
       } finally{
       real_lock.unlock();
       }
  }
/** A string representation of the clock.  This is a string of the
 * form <code>"101010 [ON]"</code> (running clock) or <code>"101010
 * [OFF]"</code> (stopped clock).  The first character of the string
 * is the value of the most significant bit of the clock.  There is
 * a single space between the last bit of the clock and the opening
 * square bracket.  {@code ON} and {@code OFF} are in uppercase.
 * The closing square bracket is the last character in the string
 * (no newline).
 *
 * @return a string representation of the clock
 */
  @SuppressWarnings("unchecked")
  @Override public String toString () {
    StringBuilder k=new StringBuilder(size);
    //can't think why we'd need to synchronize this
    //but for now lets be overly caucious
    real_lock.lock();
    try{
      for (int i=size;i>0;i--){
        if(clock.get(i-1)){
          k.append('1');
        } else {
          k.append('0');
        }
      }
      k.append((isTicking())?" [ON]":" [OFF]");
      here(k.toString());
      return k.toString();
    } finally{
      real_lock.unlock();
    }
  }
}
