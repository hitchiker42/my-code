package cs671;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;
import java.io.*;
import java.net.*;
import static java.util.Arrays.asList;
import static java.util.Collections.sort;
/**
 *Test runner. Instances of this class are used to run the tests
 *methods found in classes that implement Testable.
 *@author Tucker DiNapoli using api by Michel Charpentier
 */
public class Tester implements Runnable{
  /**
   * Indicates if the run method has been invoked on this tester
   */
  boolean hasrun=false;
  //convience macro for getting time as a double in seconds
  static double time(){return (System.nanoTime()/(Math.pow(10,9)));}
  /**
   *List to hold the results for the tests run with the tester
   */
  List<TestResult> results=new ArrayList<TestResult>();
  //Just an internal variable to hold test results for the main method
  /**
   *The Classes to be tested by the tester
   */
  ArrayList<Class<? extends Testable>> classes=new ArrayList<>();
  ArrayList<Testable> objects=new ArrayList<Testable>();
  /**
   *Output Stream for test results, defaults to Stderr
   */
  PrintWriter output=new PrintWriter(new OutputStreamWriter(System.err),true);

  //macro for my convience, really just debugging
  //its turned off, but I'm leaving it here in case I forget to
  //remove all the debug lines
  private static void println(Object text){if(1==2){System.err.println(text);}}
  /**
   *Creates a tester for the given classes
   */
  @SafeVarargs
  public Tester(Class<? extends Testable>...classes){
    //to test if Class is testable
    for (int i=0;i<classes.length;i++){
      assert(Testable.class.isAssignableFrom(classes[i]));
      this.classes.add(classes[i]);
    }
  }
  /**
   *Creates a tester for the given classes
   */
  public Tester(Collection<Class<? extends Testable>> classes){
    for (Class<? extends Testable> i : classes){
      assert(Testable.class.isAssignableFrom(i));
      this.classes.add(i);
    }
  }
  /**
   *Sets the tester output. By default, the output is System.err.
   * It is valid to set the output to null,
   * in which case the tester is completely silent.
   *@param W - the output for the tester info; can be null
   */
  public void setPrintWriter(PrintWriter W){
  //Should Note that I borrowed this code snippet from
  //http://stackoverflow.com/questions/691813/is-there-a-null-outputstream-in-java
    class NullOutputStream extends OutputStream {
      @Override
      public void write(int b) throws IOException {
      }
    }
    if(W==null){
      output=new PrintWriter(new NullOutputStream());
    }else{
      output=W;
    }
  }
    /**
     *This implementation's version of the TestResult interface.
     *Contains all fields & methods from that interface as well as
     *holding the method the test result is for and the class it is from
     */
    class Testpkg implements TestResult{
      //initalized in constructor
      Method method;double weight;String info;
      //initalized later
      boolean success;double duration;
      Object returned;Throwable error;
      Class<? extends Testable> class_;
      String name;
      /**
       *Initalizes this test result
       *@param method - The method that is being tested
       *@param weight - The weight of this test
       *@param info - The info string of the method
       *@param class_ - The class the method is in
       */
      Testpkg(Method method,double weight,String info,Class<? extends Testable> class_){
        this.method=method;this.weight=weight;
        this.info=(info.equals(""))? info : ": "+info;
        this.success=false;this.duration=0.0;this.error=null;this.returned=null;
        this.class_=class_;
        this.name=class_.getCanonicalName()+"."+method.getName();
      }
      public double getWeight(){return weight;}
      public boolean success(){return success;}
      public String getInfo(){
        return name+info;}
      public double getDuration(){return duration;}
      public Throwable error(){return error;}
    }
  /*
     Creates a subprocess to call the java decompilier on the given java class
     in order to get the correct order for methods for the run method.
   */
  /*public static ArrayList<String> getMethods(String className) {
    ProcessBuilder decomp=new ProcessBuilder("javap","-p",className);
    decomp.redirectErrorStream(true);
    ArrayList<String> methods=new ArrayList<>();
    Process javap;
    try {
      javap = decomp.start();
    } catch (IOException ex){
      methods.add("Class Not Found");
      return methods;
    }
    Scanner stdin = new Scanner(javap.getInputStream());
    Pattern p=Pattern.compile(".*\\x28.*\\x29.*;");
    String method=null;
    while(stdin.hasNextLine()){
      method=stdin.findInLine(p);
      if(method==null){
        stdin.nextLine();
      }
      else {
        methods.add(method);
        //here(method);
      }
      method=null;
    }
    methods.remove("public boolean beforeMethod (Method m) throws Exception");
    methods.remove("public void afterMethod (Method m) throws Exception");
    return methods;
    }*/

  //internal variable to hold the test results  for 1 class
  //only used as a placeholder, all tests are put into the
  //Results List eventually
  ArrayList<Testpkg> tests;
  /**
   *Run Method
   *Runs the tests. All the classes are processed in the order in which
   *they were given. For each class, all the tests are run
   *on the same instance in alphabetical order of the method names.
   *If no instance can be created, the class is skipped
   *with an error message. If method beforeMethod fails or
   *returns false, the corresponding test is skipped with
   *a warning message. After each test, method afterMethod
   *is run and a warning is displayed if it fails.
   *
   *In general, no-argument methods are valid test methods
   *if they are annotated, even when they are non-public or non-void.
   *Static method and methods that require arguments are ignored.
   *A warning is displayed if they are test-annotated.
   *@throws IllegalStateException - if this tester has already been run
   */
  public void run(){
    if (hasrun){
      throw new IllegalStateException();
    }
    for(Class<? extends Testable> foo : classes){
      //hold tests for this class
      tests=new ArrayList<Testpkg>();
      //object that we will use while testing
      Testable temp=null;
      //methods to test
      ArrayList<Method> methods_temp=new ArrayList<>(asList(foo.getDeclaredMethods()));
      ArrayList<Method> methods=new ArrayList<>();
      try{//try to initialize
        Constructor<? extends Testable> constructor=foo.getDeclaredConstructor();
        constructor.setAccessible(true);
        temp=constructor.newInstance();
      } catch(Error | Exception ex){
        output.println(String.format("ERROR Could not instantiate %s;skipped",
                              foo.toString()));
      }
      if(methods_temp.size()==0){
        output.println("ERROR: No methods found to test in class"+
                       foo.getName()+"Error");
        continue;
      }
      //Sort methods alphabetically by name
      ArrayList<String> names=new ArrayList<>();
      for (Method j : methods_temp){names.add(j.getName());}
      sort(names);
      for ( String i : names){
        for ( Method j : methods_temp){
          if (i.matches(j.getName())){
            j.setAccessible(true);
            methods.add(j);
            break;
          }
        }
      }
      for (Method meth : methods){
        //Test if method is applicable for testing,(ie. not static,
        //no parameters and can be invoked)
        try{
          meth.setAccessible(true);
        } catch(SecurityException ex){
          output.println("ERROR:Security prevents you from running your tests.");
          return;
        }
        //test if method is annotated w/test
        if(!meth.isAnnotationPresent(Test.class)){
          continue;
        }
        else{
          try{//try to get annotation parameters
            Test annotate=meth.getAnnotation(Test.class);
            ArrayList<Field> fields=new ArrayList<Field>
              (Arrays.asList(meth.getClass().getDeclaredFields()));
            for (Field i : fields){
              i.setAccessible(true);
            }
            assert(annotate.info() instanceof String);
            tests.add(new Testpkg(meth,annotate.weight(),annotate.info(),foo));
          }
          catch(IllegalArgumentException |
                NullPointerException | ClassCastException ex){
            output.println("ERROR: "+ex.toString()+" raised");
          }
        }
      }
      for(Testpkg testpkg : tests){
        //Actually run the tests
        if(testpkg.weight <=0){continue;}
        try{//invoke beforeMethod
          boolean check=temp.beforeMethod(testpkg.method);
          if (!check){
            throw new Exception();}}
        catch (Exception ex){
          output.println(String.format("WARNING:Before Method for method"
                                       +" %s has failed;ignored",
                                       testpkg.method.getName()));
          continue;}
        if (testpkg.method.getParameterTypes().length !=0){
          //test if method takes parameters
          output.println(String.format("WARNING: method %s is annotated with "
                                       +"@Test but takes parameters;ignored",
                                       testpkg.method.getName()));
          continue;
        }
        else if (Modifier.isStatic(testpkg.method.getModifiers())){
          //test if method is static
          output.println(String.format("WARNING: static method %s is annotated"
                                       +" with @Test;ignored",testpkg.method.getName()));
          continue;
        }
        try{//invoke method
          testpkg.duration=time();
          testpkg.method.invoke(temp);
          testpkg.duration=time()-testpkg.duration;
          testpkg.success=true;
        } catch(InvocationTargetException err){
          Throwable ex=err.getTargetException();
          testpkg.duration=time()-testpkg.duration;
          testpkg.error=ex;
        } catch(Exception ex){
          testpkg.duration=time()-testpkg.duration;
          testpkg.error=ex;
        } 
        try{//invoke afterMethod
          temp.afterMethod(testpkg.method);
        } catch(Error | Exception ex){
          output.println(String.format("WARNING:After Method for"
                                       +" method %s has failed",
                                       testpkg.method.toString()));
        }
        results.add(testpkg);
      }//end of for test : tests loop
    }
    hasrun=true;
  }
  /**
   *Test results. This method returns a list that contains
   * a TestResult object for each test that was run
   *(in the order the test method were actually run).
   *The returned list can be modified and modifications will
   *affect subsequent calls to getResults (i.e., this method
   *does not make copies of the list).
   *@throws IllegalStateException - if the tester has not yet been run
   */
  public List<TestResult> getResults(){
    if (!hasrun){
      throw new IllegalStateException();
    }
    return results;
  }
  /**
   *Starts a console-based application.
   *Command line arguments are the names of the classes
   *to be tested. The application produces a summary output
   *of tests that succeeded and tests that failed.
   */
  @SuppressWarnings("unchecked")
  public static void main(String[] args){
    ArrayList<Class<? extends Testable>> argClasses=new ArrayList<>();
    if (args.length<=0 || args[0]=="-h" || args[0]=="--help"){
      System.out.println("Call with one or more names of testable classes");
      return;
    }
    ClassLoader loader=ClassLoader.getSystemClassLoader();
    Class<Testable> q=Testable.class;
    Class<? extends Testable> temp=null;
    Class<?> tmp=null;
    boolean end=true;
    boolean test_able=false;
    for (String i : args){
      //see if we can load the class and if it implements testable
      try{
        tmp=loader.loadClass(i);
        for (Class<?> j : tmp.getInterfaces()){
          if(j.getCanonicalName().equals("cs671.Testable")){
            test_able=true;
          }
        }
        assert(test_able==true);
        temp=tmp.asSubclass(q);
        argClasses.add((Class<? extends Testable>)tmp);
        end=false;
      } catch(ClassCastException ex){
        end=true;
        System.err.println("ERROR:Class "+i+" does not implement Testable");
      } catch(ClassNotFoundException ex){
        end=true;
        System.err.println("ERROR:Class "+i+" not found");
      } catch(Error | Exception ex){
        end=true;
        //ex.printStackTrace();
        //System.err.println(ex.getMessage());
        System.err.println(String.format("Args: %s\nTemp: %s\nClasses: %s"
                              ,args.toString(),temp.toString(),
                              argClasses.toString()));
      }
      if(end == true){
        return;
      }
    }
    if (argClasses == null){
      System.err.println("ERROR: No classes found to test");
      return;
    }
    if (end==false){
      List<Testpkg> main_results=new ArrayList<Testpkg>();
      Tester testRun=new Tester(argClasses);
      testRun.run();
      for (TestResult i:testRun.getResults()){
        main_results.add((Testpkg)i);
      }
      ArrayList<Testpkg> pass=new ArrayList<>();
      ArrayList<Testpkg> fail=new ArrayList<>();
      double score=0;double total=0;
      for (Testpkg i : main_results){
        total+=i.getWeight();
        if (i.success){
          pass.add(i);
          score+=i.getWeight();
        } else{
          fail.add(i);
        }
      }
      System.out.println("SUCCESSFUL TESTS:");
      for (Testpkg i : pass){
        System.out.println("  "+i.getInfo()+
                           " "+String.format("(%.1f)",i.getWeight())+" in "+
                           String.format("%.5f",i.getDuration()*Math.pow(10,-3))+
                           " milliseconds");
      }
      System.out.println("FAILED TESTS:");
      for (Testpkg i : fail){
        System.out.println("  "+i.getInfo()+" "+String.format
                           ("(%.1f)",i.getWeight())+" from "+
                           i.error.toString());
      }
      System.out.println(String.format("SCORE = %.2f%%",(score/total)*100));
      return;
    }
  }
}
