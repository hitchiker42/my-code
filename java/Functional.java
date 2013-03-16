package cs671;
import java.util.*;
import java.lang.reflect.*;
import java.util.concurrent.atomic.AtomicBoolean;
//m4_define('clone','new $1 _$1_ = ($1)$2.clone();')dnl
//m4_define(`exec',`static $1($@){}')dnl args of form(name,'type param'...)
import sun.misc.*;
import java.lang.reflect.*;
@SuppressWarnings("Unsafe")
class unsafe{
  public static Unsafe getUnsafe() {
    try {
      Field f = Unsafe.class.getDeclaredField("theUnsafe");
      f.setAccessible(true);
      return (Unsafe)f.get(null);
    } catch (Exception e) {
      System.err.println("FAIL");
      System.exit(1);}
    return null;
  }
}
@SuppressWarnings("unchecked")
class Functional{
  static Method clone;
  void set(){
    try{
    clone=Object.class.getMethod("clone");
    clone.setAccessible(true);
    }catch(Exception ex){}
  }
  class immutable<T>{
    final T x;
    private immutable(T x){
      this.x=x;
    }
    T get(){
      try{
      return (T)clone.invoke(x);
      } catch (Exception ex){}
      return null;
    }
  }
  class Immutable_BitSet{
    private final BitSet x;
    private Immutable_BitSet(BitSet x){
      this.x=x;
    }
    BitSet val(){
      return (BitSet)x.clone();
    }
    long size(){
      return x.size();
    }
    boolean get(int n){
      return x.get(n);
    }
    BitSet set(int n){
      BitSet temp=this.val();
      temp.set(n);
      return temp;
    }
    BitSet clear(int n){
      BitSet temp=this.val();
      temp.clear(n);
      return temp;
    }
    BitSet flip(int n){
      BitSet temp=this.val();
      temp.flip(n);
      return temp;
    }
  }
}
  //class AtomicBooleanArray{
    //int max 2147483647=(01...11)
    //int min -2147483648=(10...00)
    //int 0, 0=(00...00)
    //int 1, -1=(11...11)
    //long max 9223372036854775807=01...11
    //long max-1=01....10,max-2=01...01
    //long min -9223372036854775808=10...00
    //long min+1=10...01,min+2=10...10
  //}