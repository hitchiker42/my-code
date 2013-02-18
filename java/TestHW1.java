

package tests;

import charpov.grader.*;
import java.io.FileOutputStream;
import java.io.PrintWriter;

class TestHW1{
    public static void main (String[] args) throws Exception {
        java.util.logging.Logger.getLogger("charpov.grader")
          .setLevel(java.util.logging.Level.WARNING);
        //Tester tester = new Tester(TestLiar.class);
          Tester tester=new Tester(TestGuesserTextUI.class, TestHiLo.class, TestLiar.class);
	FileOutputStream fos = new FileOutputStream("run1.out");
        tester.setOutputStream(fos);
        double result = tester.call();
        PrintWriter pw = new PrintWriter(fos, true);
        pw.printf("Total: %s/%s%n", result * 100, 100);
    }
}
