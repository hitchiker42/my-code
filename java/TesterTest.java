package cs671;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;

public class TesterTest implements Testable{
  static void println(Object text){System.err.println(text);}
  public boolean beforeMethod(Method m) throws Exception{
    if (m.getName().equals("brokenMethod")){
      throw new Exception();
    } else if (m.getName().equals("isFalse")){
      return false;
    } else{
      println("Before Method executed");
      return true;
    }
  }
  public void afterMethod(Method m) throws Exception{
    if (m.getName().equals("failAfter")){
      throw new Exception();
    } else {
      println("After method executed");
      return;
    }
  }
  @Test(weight=1, info="null")
  Object nullTest(){return null;}
  @Test(weight=0, info="weightless")
  void weightless() {return;}
  @Test(weight=-100, info="neg-weight")
  void negWeight() {return;}
  @Test(info="broken")
  void brokenMethod () {return;}
  @Test(info="fail After")
  void failAfter () {return;}
  @Test(info="exception")
  void eXception() throws Exception,RuntimeException {
    if(1==1){
      throw new Exception ();
    }
    if(2==2){
      throw new RuntimeException ();
    }
    return;}
  @Test(weight=10000000,info="massive")
  boolean massive () {return true;}
  @Test(info="Static")
  static void sTaTic () {return;}
  @Test(info="parameteterized")
  void parameter (String w){return;}
  @Test(info="printer")
  void printer (){
    System.out.println("Hello, World!");
    return;}
  @Test(info="return")
  String hello () {return "Hello,World!";}
  @Test(info="false")
  void isFalse () {return;}
  @Test(info="private")
  private void hidden () {return;
  }
}