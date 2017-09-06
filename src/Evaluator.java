import java.io.File;

import java.util.Scanner;
import java.util.Random;
import java.util.Arrays;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.XMLNode;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.A4TupleSet;
import edu.mit.csail.sdg.alloy4compiler.translator.A4SolutionReader;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;


/* TODO: Now we cannot generate mappings for known points in a set
 * because we cannot tell of "p$0" is something new, or just an alias
 * for an existing point "p". Find a way to figure this out. The idea 
 * is: know what the identifiers are somehow, and do a lookup everytime
 * outputing names. If a name exists, just throw out "$0".
 */

public class Evaluator {

    // private static String model = "sig Point {} \n" + "\n" + "run { #Point > 1 } for 3 but 3 Int";
    // private static String outputfilename = "/tmp/myissue.xml";
    // public static final int NUM_INSTANCES = 5;
    private static Random rnd;

    // Printing solutions randomly
    public static void printSols(ArrayList<String> sols, int numInstances) {
        for(int i = 0; i < numInstances; i++) {
            int index = rnd.nextInt(sols.size());
            System.out.println(sols.get(index));
            sols.remove(index);
        }
    }

    // Main function to run Alloy Analyzer
    public static ArrayList<String> run(String filename,  String[] targets) throws Exception {
        A4Reporter rep = new A4Reporter();
        ArrayList<String> solStrings = new ArrayList<String>();
        Module world = CompUtil.parseEverything_fromFile(rep, null, filename);
        A4Options options = new A4Options();
        options.solver = A4Options.SatSolver.SAT4J;
        // options.symmetry = 0; // optionally turn off symmetry breaking
        for (Command command: world.getAllCommands()) {
            // Execute the command
            A4Solution sol = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
            int i = 0;
            if(i == 0 != sol.satisfiable()) {
                System.out.println("The predicates are not satisfiable. No instance generated.");
                System.exit(-1);
            }
            // while (sol.satisfiable() && i < numInstances) {
            while (sol.satisfiable()) {
                // System.out.println("[Solution]:");
                // System.out.println(sol.toString());
                // sol.writeXML("bijection.xml");
                String curSolStr = "";
                for(String f : targets) {
                    // System.out.println(sol.eval(e));
                    Expr e = CompUtil.parseOneExpression_fromString(world, f);
                    //  If this solution is solved and satisfiable, evaluates the 
                    //  given expression and returns an A4TupleSet, a java Integer, or a java Boolean.
                    A4TupleSet tups = (A4TupleSet) sol.eval(e);
                    /* TODO: do we want to skip empty set solutions or not??
                    if(tups.size() == 0) {
                    }
                    */
                    curSolStr += (f + ": " + tups.toString() + "\n");
                }
                solStrings.add(curSolStr);
                sol = sol.next();
                i++;
            }
        }
        return solStrings;
    }

    public static void main(String[] args) throws Exception {
        if(args.length < 3) {
            System.out.println("usage: <model-filename> <number-of-instances> <list-of-target-functions>");
            System.exit(-1);
        }
        rnd = new Random(System.currentTimeMillis());
        String[] targets = Arrays.copyOfRange(args, 2, args.length);
        printSols( run(args[0],  targets), Integer.parseInt(args[1]) ); 
    } 
}
