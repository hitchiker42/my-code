//For the guesser, might need to use g, and might need to call initalizer while parsing
//command line arguements
package cs671;
import java.io.*;
import java.util.*;//For now, but simplify to only classes I need
/*Main Text Interfact to guesser program*/
/**
 *A simple, text-based user interface for guessing games. This interface is
 *constructed from a Guesser object and offers a play method. This method is
 *blocking (i.e., it does not return until the user is done playing) and
 *returns the number of games played. 
 *This class also implements a command-line program to start a HiLo or a Liar
 *guessing session
 *@author Tucker DiNapoli from api by Michel Charpentier
 */
public class GuesserTextUI {
  //I'm lazy and this makes printing things eaiser
  private static void println(String arg){
    System.out.println(arg);
  }
  //intput stream
  BufferedReader input;
  //output stream
  PrintWriter output;
  //guesser instance
  Guesser<?> guesser;
  //string containing question to be asked
  String question;
  //string containing users answer
  String answer;
  /**
   *Builds a user interface for the given guesser using stdout/stdin
   */
  public GuesserTextUI(Guesser<?> g){
    this.output=new PrintWriter(new OutputStreamWriter(System.out),true);
    this.input=new BufferedReader(new InputStreamReader(System.in));
    this.guesser=g;
  }
  /**
   *Builds a user interface for the given guesser. 
   *Questions are displayed on output and user input 
   *is read from input.
   */
  public GuesserTextUI(Guesser<?> g, Reader input, Writer output){
    this.output=new PrintWriter(output);
    this.input=new BufferedReader(input);//may change
    this.guesser=g;
  }
  /**
   *Starts the interaction with the user. This method will only return when the
   *user is done playing. 
   *'y' (followed by a newline) is accepted as a valid input for 'yes';
   *similarly, 'n' means 'no'. This implementation accepts the answers 'y' 'n'
   *'yes' and 'no' with any captialization as valid answers  Invalid answers
   *from the user do not end the interaction. Instead, the method keeps asking
   *until it gets a yes or no. 

   *Upon termination, this method flushes the output writer but does not close
   *it, nor does it close the input reader.
   *@return the number of games played
   */
  public int play(){
    boolean play=true;
    int games=0;
    while (play==true){
      guesser.initialize();
      while (guesser.hasSolved() != true ){
        question=guesser.makeQuestion();
        while(true){
          output.println(question);
          try{
            answer=input.readLine();
          } catch (IOException ex){
            println("Input somehow failed");
            return 0;
          }
          if (answer.toLowerCase().equals("y") || answer.toLowerCase().equals("yes")){
            guesser.yes();
            break;
          } else if (answer.toLowerCase().equals("n") || answer.toLowerCase().equals("no")){
            guesser.no();
            break;
          }
          else {
            //prompt for another answer
            continue;
          }
        }
        double progress=guesser.progress();
        output.println(String.format("I am %.0f %% complete",100*progress));
      }
      String secret=guesser.getSecret().toString();
      output.println(String.format("Your secret was %s",secret));
      games++;
      while(true){
        output.println("Play Again?(y/n)");
        try{
          answer=input.readLine();
        } catch (IOException ex){
          println("Input somehow failed");
          return 0;
        }
        if (answer.toLowerCase().equals("y") || answer.toLowerCase().equals("yes")){
          play=true;
          break;
        } else if (answer.toLowerCase().equals("n") || answer.toLowerCase().equals("no")){
          play=false;
          break;
        } else{
          continue;
        }
      }
    }
    output.println(String.format("You played %d games",games));
    return games;
  }
  /**
   *help method
   *How to use the Program, prints on -h, [-]-help or no args
  */
  static void help() {
    //personally it would bother me if I couldn't call
    //a program with -h and get help
    //thus the -h/--help parameters
    String usage="Usage: TextUI -hilo min max\n"
      +"or: TextUI -liar #lies name1 [name2 ...]\n"
      +"or: TextUI -liar #lies -file filename #names\n"
      +"Options: -h,[-]-help print this help and exit\n";
    System.out.println(usage);
    return;
  }
  /**
   *Starts a command-line program. This program can be started in 3 different
   *ways: 
   *Usage: TextUI -hilo min max
   *or: TextUI -liar #lies name1 [name2 ...]
   *or: TextUI -liar #lies -file filename #names
   *The last form takes the names of the secret objects from a file and the last
   *parameter specifies how many of these are actually used in the game. 
   *@throws IOException - if the file of names can not be opened and read
   */
  public static void main(String [] args) throws IOException{
    if (args.length <= 0 || args[0].equals("-h") || args[0].equals("--help") || args[0].equals("-help")){
      help();
    } else if (args[0].equals("-liar")){
      //liar cases
      if (args.length<3){
        System.out.println("Not Enough Arguements");
        return;
      }
      HashSet<String> candidates = new HashSet<>();
      /*I can think of no reasonably implimentable means of finding
        the type of the lies, so they are all going to be strings*/
      String name="String";
      //I know I could probably clean this up as there is a lot
      //of duplicate code for creating a liar instance with
      //or without a file, but as there are only 2 cases and
      //they're already written out it would be more trouble
      //than it's worth to attempt to abstract it
      //using a file
      if (args[2].equals("-file")){
        if (args.length != 5){
          println("If using a file you need 5 arguments of the form:\n"+" -liar #lies -file filename #names");
          return;
        }
        try{
          int lies= new Integer(args[1]);
          File file=new File(args[3]);
          assert(file.exists()==true);
          BufferedReader fileRead=new BufferedReader(new FileReader(file));
          String word=fileRead.readLine();
          while (word!=null){
            candidates.add(word);
            word=fileRead.readLine();
          }
          Liar<String> liar=new Liar<>(candidates,lies,name);
          try{
            int n=liar.selectCandidates(new Integer(args[4]));
            if(n<0){
              return;
            }
          } catch(NumberFormatException ex){
            println("# of canditates to use not an int");
            return;
          }
          GuesserTextUI game=new GuesserTextUI(liar);
          game.play();
        }  catch(FileNotFoundException ex){
          println("File not found");
          return;
        } catch(IOException ex){
          println("IO exception");
          return;
        }
      } else{
        //using stdin/stdout
        try{
          int lies= new Integer(args[1]);
          for (int i=2;i<args.length;i++){
            candidates.add(args[i]);
          }
          println(candidates.toString());
          GuesserTextUI game=new GuesserTextUI(new Liar<>(candidates,lies,name));
          assert(!game.equals(null));
          game.play();
        } catch(NumberFormatException ex){
          println("# of lies not an int");
          return;
        }
      }
    } else if (args[0].equals("-hilo")){
      //hilo case
      if(args.length < 2){
        help();
      } else try{
          int min= new Integer(args[1]);
          int max= new Integer(args[2]);
          GuesserTextUI game=new GuesserTextUI(new HiLo(min,max));
          game.play();
        } catch(NumberFormatException ex){
          println("Arguments were not ints");
          return;
        }
    }
    else {
      //should rarely get here as most incorect args are delt with in
      //the hilo and liar cases and no args prints a help message
      println("ERROR");
      return;
    }
  }
}