package cs671;
/**
 *A simple guessing game. This game illustrates the Guesser interface. The user
 *picks a number between a specified minimum and maximum and the guesser will
 *discover the number by a repeated interaction of YES/NO questions. It proceeds
 *by dichotomy, splitting the range of possible values in half with each
 *question. After it's found, the number is returned as an instance of Integer. 
 *@author Tucker DiNapoli from api by Michel Charpentier
 */
public class HiLo implements Guesser<Integer> {
  //min max and number to ask, min & max are changeable over
  //each game
  private int min,max,x;
  double total;
  Range range;
  private boolean init=false;
  private boolean solved=false;
  private boolean qmade=false;
  /**
   *Range of values, kept via min and max values.
   *Really simple class to hold the minimum and maximum values
   *over multiple games.
   */
  private static class Range {
    static int min;
    static int max;
    Range(int min,int max){
      Range.min=min;
      //seems a bit cheap to do this, but because abs(int_max)=abs(int_min)-1
      //it turns out to be necessary
      if (min==Integer.MIN_VALUE){
        Range.min=min+1;
      }
      Range.max=max;
    }
  }
  /**
   *Builds an instance of the engine for the given minimum and maximum.
   *@param min - the lower bound of the range
   *@param max - the upper bounh of the range
   *@throws IllegalArgumentException - if the specified range is empty, i.e.,
   * min> max
   */
  public HiLo(int min,int max){
    if (min>max){
      throw new IllegalArgumentException();
    }
    this.max=max;
    this.min=min;
    range= new Range(min,max);
    this.x=(int)(((long)min+(long)max)/2+((long)min+(long)max)%2);
    this.total=(double)((long)max-(long)min);
  }
  /**
   *{@inheritDoc}
   */
  public Integer getSecret(){
    if (solved != true){
      throw new IllegalStateException() ;
    }
    solved=false;
    return x;
  }
  /**
   *{@inheritDoc}
   */
  public boolean hasSolved(){
    if (init!=true){
      throw new IllegalStateException();
    }
    if (this.min == this.max){
      x=min;
      assert(x==max);
      solved=true;
      return true;
    } else {
      return false;
    }
  }
  /**
   *Initalizes the guesser. This method must be the first called after the
   *guesser is created.
   *@return the string "Pick a number between X and Y", where X is the minimum
   *and Y is the maximum, as specified when the instance was constructed 
   */
  public String initialize(){
    max=Range.max;
    min=Range.min;
    init=true;
    return String.format("Pick a number between %d and %d",min,max);
  }
  /**
   *Generates a new question. The previous question must be answered (using yes
   *or no) before a new question is generated.
   *@return the question as a string to display to the user
   */
  public String makeQuestion(){
    if (init != true || solved==true || qmade==true){
      throw new IllegalStateException();
    }
    qmade=true;
    x=(int)(((long)min + (long)max)/2);//+((long)min + (long)max)%2); //rounds up
    if(min<0 || max<0){
      x=(int)((long)min + (long)max)/2;
    }
    String question=String.format("Is your number larger than %d",x);
     return question;
  }
  /**
   *{@inheritDoc}
   */
  public void no(){
    if (init!=true || qmade!=true){
      throw new IllegalStateException();
    }
    qmade=false;
    this.max=x;
    //negitive numbers only
    if (Math.abs(max)>Math.abs(Range.min) && max<0){
      max=Range.min;
    }
  }
  /**
   *{@inheritDoc}
   */
  public void yes(){
    if (init!=true || qmade!=true){
      throw new IllegalStateException();
    }
    qmade=false;
    this.min=(x+1);
  }
  /**
   *Indicates progress towards solving the problem. After initialization, the
   *value returned is 0 (unless the problem is immediately solved, in which case
   *it is 1). It always increases as the guessing process progresses. It is
   *exactly 1 after the problem is solved.
   *<p>
   *This implementation measures progress by number of items remaining over
   *the total number of items. Which will cause the growth of progress to
   *exponentally decay as the amount of items decrease
   */
  public double progress(){
    //scaling of progress is a bit off because it uses remaining/total
    //but it works and so thats the way I'm leaving it.
    if (init!=true){
      throw new IllegalStateException();
    }
    if (min==max){
      return(1.0);
    }
    Double current=(double)((long)max-(long)min);
    return(1-current/total);
  }
}
