package cs671;
import java.util.*;
import java.util.Collections.*;
import java.util.EnumSet.*;
  /**
   *Liar Class<br>
   *A guessing game for liars.
   *This class implements a guesser that asks questions to a user to find
   *a secret object. The user can lie, but no more than a
   *predetermined  number of times. When found,
   *the secret object is returned as an instance of Liar.Secret,
   *which includes the number of times
   *the user lied during the process.
   *@author Tucker DiNapoli from api by Michel Charpentier
   */
public class Liar<T> implements Guesser<Liar.Secret<T>>{
  //internal variables to insure methods are called in the proper order
  private boolean init=false;
  private boolean qmade=false;
  private boolean solved=false;
  int asked=0;
  static int min(int a,int b){
    return(a<b ? a : b);
  }
  //This is just a macro for myself, to make printing eaiser
  //mainly for debugging
  private static void println(String arg){
    System.out.println(arg);
  }
  //instance of the LiarTable class
  LiarTable liarTable;
  //internal variable to hold the given list of candidates
  ArrayList<T> candidates;
  //internal variable to hold the number of candidates
  int items;
  /**
   *Number of times a user can lie
   */
  public final int maxLies;
  /**
   *Name of objects
   */
  public final String name;
  /**
   *Tests whether the problem is solved or not.
   *@return true iff the problem has been solved
   */
  public boolean hasSolved(){
    if(!init){
      throw(new IllegalStateException());
    }
    return liarTable.hasSolved();
  }
    /**
     *Indicates progress towards solving the problem. After
     *initialization,  he value returned is 0 (unless the problem is
     *immediately solved, in which case it is 1). It always increases
     *as the guessing process progresses. It is exactly 1 after the
     *problem is solved.
     *@return a double from 0.0 to 1.0 indicating progress toward solving the
     *problem
   */
  public double progress(){
    if (solved==true){
      return 1.0;
    }
    return(1-(liarTable.progress()/liarTable.total));
  }


  //Type of Secrets, just used an interface to outside
  //All internal manipulations done w/LiarTable
    /**
     *The type of secrets produced by this guesser. Secrets are simply a pair:
     *secret object and number of times the user lied. 
     *@param E - the type of secret objects, which is T, the type parameter
     *of the Liar class. 
   */
  public static class Secret<E> {
    int lies;
    E secret;
    Secret(int lies,E secret){
      this.lies=lies;
      this.secret=secret;
    }
    /**
     *The number of times the user lied
     */
    public int getLies(){
      return lies;
    }
    /**
     *The secret that was discovered, of type T
     */
    public E getSecret(){
      return secret;
    }
  /**
   *the string representation of secret followed by " (with X lies)", where X >
   *1 is the number of times the user lied, or by " (with 1 lie)" or by " (with
   *no lie)" 
   */
    public String toString(){
      if (lies>1) {
      return secret.toString()+ String.format(" with %d lies",lies);
      } else if (lies==1){
        return secret.toString()+ String.format(" with %d lie",lies);
      } else if (lies==0){
        return secret.toString()+" with no lies";
      }
      return "";
    }
  }


  /**
   *Data structure to hold information about items and lies.
   *Does all the manipulating and sorting of objects.
   *Is a generic class, but will always be same class as liar instance
  */
  class LiarTable {
    TreeMap<Integer,ArrayList<T>> top;
    TreeMap<Integer,ArrayList<T>> bottom;
    TreeMap<Integer,ArrayList<T>> liarList;
    double total;//# of total lies at star
    double sum_last=total;
    /**
     *LiarTable Constructor<br>
    */
    LiarTable(ArrayList<T> candidates,int lies){
      this.top=new TreeMap<>();
      this.bottom=new TreeMap<>();
      this.top.put(0,new ArrayList<T>(candidates.subList(0,items/2)));
      this.bottom.put(0,new ArrayList<T>(candidates.subList(items/2,items)));
      for (int i=1;i<=lies;i++){
        this.top.put(i,new ArrayList<T>());
        this.bottom.put(i,new ArrayList<T>());
      }
      this.total=items*(lies+1);
      //println(this.top.toString()+this.bottom.toString()+'\n');
    }

    /**
     *Increment lies on top/bottom
     *
     *Given a boolean based on the users previous answer
     *increments the lie count on the group of items the user's
     *item was claimed not to be in, if an item is over the
     *max lies it is removed from the pool of possible choices.
    */
    @SuppressWarnings("unchecked")
    void increment(boolean inTop){
      TreeMap<Integer,ArrayList<T>> liarList;
      if (inTop==true){
        liarList=bottom;
      } else {
        liarList=top;
      }
      ArrayList<T> temp;
      for (int i=min(maxLies,asked);i>0;i--){
        try{
          //warnings suppressed for this,we're casting a clone of
          //an ArrayList<T> into an ArrayList<T>, so it should
          //always work, not sure why its necessary but clone
          //just returns a plain object
          temp=(ArrayList<T>)liarList.get(i-1).clone();
          liarList.put(i,temp);
        } catch(ClassCastException ex){
        }
      }
      //temp=liarList.get(0);...keeping just in case
      liarList.get(0).clear();
    }

    /**
     *Swap part of top and bottom w/each other
     *
     *The items left as possible correct answers are
     *distributed over the top and bottom maps in such
     *a way that there will always be items on the top and
     *bottom maps and that the questions will not fall
     *into an infinite loop
    */
    void swap(){
      //move the last n/2 items from top to temp array
      //move the first n/2+n%2 items from bottom to temp array
      //merge top and bottom, this is new top
      //temp array is new bottom
      ArrayList<T> up;
      ArrayList<T> down;
      ArrayList<T> temp;
      boolean up_put=false;
      for (int i=0;i<=min(maxLies,asked);i++){
        up=top.get(i);
        down=bottom.get(i);
        temp=new ArrayList<>(up.size());
        //manipulation with temp variables
        if (down.size()==1){
          up.addAll(down);
          down.clear();
        } else if (up.size()==1 && up_put==false){
          up_put=true;
          continue;
        } else {
          temp.addAll(up.subList(up.size()/2,up.size()));
          up.subList(up.size()/2,up.size()).clear();
          temp.addAll(down.subList(0,(down.size()/2+down.size()%2)));
          down.subList(0,(down.size()/2+down.size()%2)).clear();
          up.addAll(down);
        }
        //change real variables
        top.put(i,up);
        bottom.put(i,temp);
      }
    }

    /**
     *return progress as a double giving the total number
     *of possible lies left, this is compaired w/the value
     *total ,which gives total lies ever possible, to get
     *the actual progress value.
     *@return progress as a decimal from 0.0 to 1.0
    */
    double progress(){
      //sum lies left per object over all objects
      if(!init){
        throw new IllegalStateException();
      }
      double sum=0;
      for(int i=0;i<=min(maxLies,asked);i++){
        sum += top.get(i).size()*(maxLies-(i-1));
        sum += bottom.get(i).size()*(maxLies-(i-1));
      }
      if(sum==sum_last){
        sum-=.5;
      }
      sum_last=sum;
      return sum;
    }

    /**
     *Fail fast method of determining if the item has
     *been found. sums the size of the arrays in each
     *item in the top and bottom maps
     *if the sum is ever greater than 1 it returns 
     *immedidately
     * @return true if the item found false otherwise
    */
    boolean hasSolved(){
      int sum=top.get(0).size()+bottom.get(0).size();
      for (int i=1;i<=maxLies;i++){
        if (sum>1){
          return false;
        }
        sum+=(top.get(i).size()+bottom.get(i).size());
      }
      if (sum>1){
        return false;
      } else if (sum==1){
        solved=true;
        return true;
      } else {
        println("What the hell happened");
        System.exit(89);
        return true;
      }
    }
  }

  //Constructor
  /**
     *Builds a guesser that will find the secret object among a list of
     *candidates, assuming  a maximum number of lies.
     *@param candidates - a list of objects, which includes the secret to be
     *found
     *@param lies - the maximum number of times a use can lie
     *@param name - a word that's used to refer to the objects from the list
     *(string,  object, animal, color, ...)
     *@throws IllegalArgumentException - if the set of candidates is empty, the
     *number of lies is negative, or the name is null
   */
  public Liar(Set<? extends T> candidates, int lies, String name){
    items=candidates.size();
    try{
      //check if candidates is empty
      if (candidates.size() == 0){
        throw new IllegalArgumentException("empty candidates list");
      }
      //check if lies <0
      if (lies < 0){
        throw new IllegalArgumentException("Can't Have negitive lies");
      }
      //check if name == null
      if (name.equals(null)){
        throw new IllegalArgumentException("Unspecified Name");
      }
      } catch (IllegalArgumentException ex){
      println(ex.getMessage());
    }
    this.maxLies=lies;
    this.name=name;
    this.candidates=new ArrayList<>(candidates);
    this.liarTable=new LiarTable(this.candidates,lies);
  }
  //Get Secret
  /**
   *The answer to the problem. This method should be called after hasSolved
   *returns true to retreive the solution to the problem.
   *@return the answer to the problem, if known
   */
  public Secret<T> getSecret(){
    if (init!=true || solved!=true){
      throw(new IllegalStateException());
    }
    Secret<T> ans=null;
    for (int i=0;i<=min(maxLies,asked);i++){
      if (!liarTable.top.get(i).isEmpty()){
        ans=new Secret<>(i,liarTable.top.get(i).get(0));
        break;
        }
      if (!liarTable.bottom.get(i).isEmpty()){
        ans=new Secret<>(i,liarTable.bottom.get(i).get(0));
        break;
        }
    }
    solved=false;
    return(ans);
  }

  /**
   *{@inheritDoc}
   */
  public String initialize(){
    //reset liarTable
    this.liarTable=new LiarTable(this.candidates,maxLies);
    init=true;
    if (candidates.size() == 1){
      solved=true;
    }
    return "";
  }

  /**
   *Generates a new question. The previous question must be answered (using yes
   *or no) before a new question is generated.
   *@return the question, as a string to display to the user
   */
  public String makeQuestion(){
    if (init!=true || qmade == true || solved == true){
      throw(new IllegalStateException());
    }
    StringBuilder question = new StringBuilder(String.format("Is the secret %s among ",name));
    question.append(liarTable.top.values());
    /*for (ArrayList<T> i:liarTable.top.values()){
      Iterator<T> q = i.iterator();
      while (q.hasNext()){
        question.append(q.next());
        question.append(' ');
      }
    }*/
    //question.deleteCharAt(question.length()-1);
    question.append('?');
    qmade=true;
    return question.toString();
  }
  /**
   *{@inheritDoc}
   */
  public void no(){
    if (init!=true || qmade!=true){
      throw(new IllegalStateException());
    }
    //add 1 lie to each entry in top and remove any with lies>maxlies
    qmade=false;
    asked ++;
    liarTable.increment(false);
    liarTable.swap();
  }

  /**
   *{@inheritDoc}
   */
  public void yes(){
    if (init!=true || qmade!=true){
      throw(new IllegalStateException());
    }
    //add 1 lie to each entry in bottom and remove any with lies>maxlies
    qmade=false;
    asked ++;
    liarTable.increment(true);
    liarTable.swap();
  }
  /**
   *Sets an upper bound on the number of objects to be used in the guessing
   *process. By default,  all the objects provided at construction time are
   *used. This methods selects a randomly chosen subset of objects of the
   *specified size. The selection relies on java.util.Collections.shuffle for
   *randomness.
   *<p>
   *For instance, if a Liar object is created with candidates {A,B,C,D} and
   *selectCandidates(2) is called, 2 candidates are chosen (say, A and C) and
   *used for the next game. B and D are then not used at all and won't appear in
   *any question. selectCandidates() should be called after a game has finished
   *and before the next game is initialized. If called at other times, it throws
   *an IllegalStateException.
   *<p>
   *The method is most useful when a Liar instance is created with a large set
   *(e.g., a dictionary of all English words) but we care to play with only a
   *few elements from this set (e.g., 10 words, chosen randomly). 
   */
  public int selectCandidates(int n){
    if(init){
      throw new IllegalStateException();
    }
    if (n<=0){
      throw new IllegalArgumentException();
    } else if (n>=candidates.size()){
      return candidates.size();
    }
    Collections.shuffle(candidates);
    candidates=new ArrayList<T>(candidates.subList(0,n));
    items=n;
    return n;
  }
}
