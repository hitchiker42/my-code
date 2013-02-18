package cs671;
import java.io.*;
class Debug
{
  final static boolean DEBUG = true;
  static FileWriter logger;
  static PrintWriter tracer;
  static void here (){
    if (DEBUG) {
      String fullClassName = Thread.currentThread().
        getStackTrace()[2].getClassName();
      String className = fullClassName.
        substring(fullClassName.lastIndexOf(".") + 1);
      String methodName = Thread.currentThread().
        getStackTrace()[2].getMethodName();
      int lineNumber = Thread.currentThread().
        getStackTrace()[2].getLineNumber();
      String message=("Here at: "+className + "."+ 
                      methodName + "():" + lineNumber);
      System.err.println(message);
      try{
        logger=new FileWriter("debug.log",true);
        logger.write(message+"\n");
        logger.close();}
      catch(IOException ex){}
    }
  }
  static void here (String here_string){
    if (DEBUG) {
      String fullClassName = Thread.currentThread().
        getStackTrace()[2].getClassName();
      String className = fullClassName.
        substring(fullClassName.lastIndexOf(".") + 1);
      String methodName = Thread.currentThread().
        getStackTrace()[2].getMethodName();
      int lineNumber = Thread.currentThread().
        getStackTrace()[2].getLineNumber();
      String message=("Here at: "+className + "."+ 
                      methodName + "():" + lineNumber + " "+here_string);
      System.err.println(message);
      try{
        logger=new FileWriter("debug.log",true);
        logger.write(message+"\n");
        logger.close();}
      catch(IOException ex){}
    }
  }
  static void log (){
    String fullClassName = Thread.currentThread().
      getStackTrace()[2].getClassName();
    String className = fullClassName.
      substring(fullClassName.lastIndexOf(".") + 1);
    String methodName = Thread.currentThread().
      getStackTrace()[2].getMethodName();
    int lineNumber = Thread.currentThread().
      getStackTrace()[2].getLineNumber();
    String message=(className + "."+ 
                    methodName + "():" + lineNumber);
    try{
      logger=new FileWriter("debug.log",true);
        logger.write(message+"\n");
        logger.close();
    }
    catch(IOException ex){}
  }
  static void trace (Throwable trace){
    try{
      tracer=new PrintWriter(new FileWriter("debug.log",true),true);
      tracer.println(trace.getLocalizedMessage());
      trace.fillInStackTrace();
      trace.printStackTrace(tracer);
    }
    catch(IOException ex){}
  }
}
