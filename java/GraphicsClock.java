package cs671;
import java.awt.event.*;
import java.util.*;
/** Graphical representation of a binary clock.
 *
 * @author  Michel Charpentier
 * @version 3.1, 02/12/13
 * @see Clock
 */
public class GraphicsClock extends javax.swing.JComponent implements Observer {

  private static final long serialVersionUID = -4385200557405026128L;

  private final Dot[] dots;
  private final Clock clock;

  /** Builds a graphical representation of the given clock.
   *
   * @param clock the clock to be displayed
   * @param width the width of the component
   */
  public GraphicsClock (Clock clock, int width) {
    this.clock = clock;
    int nbBits = clock.size();
    dots = new Dot[nbBits];
    double r = (width - 10) / (2.25 * nbBits);
    if (r < 10) r = 10;
    double y = r * 1.25;
    double x = 2.25 * r;
    for (int i=0; i<dots.length; i++) {
      Dot d = dots[i] = new Dot(r*1.25+i*x, y, r);
      if (clock.getBit(nbBits - i - 1))
        d.set();
    }
    setPreferredSize(new java.awt.Dimension(width,(int)(1.1*width/nbBits)));
  }

  /** Paints the clock as a line of big dots.
   * @see <a href="Dot.java">Dot.java</a>
   */
  protected void paintComponent (java.awt.Graphics  g) {
    for (Dot dot : dots)
      dot.paint(g);
  }
  class ClockListener implements MouseListener{
    //This V is why interfaces are a ridculous concept
    public void mouseEntered(MouseEvent e){}
    public void mouseExited(MouseEvent e){}
    public void mouseReleased(MouseEvent e){}
    public void mousePressed(MouseEvent e){}
    public void mouseClicked(MouseEvent e){
      switch(e.getButton()) {
      case(1):
        if (e.getClickCount()==1){
          for (Dot d : dots){
            if (d.contains(e.getX(),e.getY())){
              d.flip();
              break;
            }
          }
        } else if (e.getClickCount()==2){
          if (e.isShiftDown()){
            clock.setLongValue(Clock.time());
          } else{
            clock.clear();
          }
        } else{}
        break;
      case(2):
        clock.setDirection((clock.getDirection()==Clock.Direction.FORWARD)?Clock.Direction.BACKWARD:Clock.Direction.FORWARD);
        break;
      case(3):
        if(clock.isTicking()){
          clock.stop();
        } else{clock.start();}
        break;
      }
    }
  }
  @Override
  public void update(Observable o, Object arg){
    Clock c=null;BitSet b=null;
    int i=0,j=0;
    try{
      c=(Clock)o;
      b=(BitSet)arg;
    } catch(ClassCastException ex)
      {}
    while((j=b.nextSetBit(i))!=-1){
      i=j;
      dots[j].flip();
    }
  }
}
