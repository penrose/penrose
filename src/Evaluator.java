import java.io.File;

import java.util.Scanner;
import java.util.Arrays;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.XMLNode;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.A4SolutionReader;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;

public class Evaluator {

    private static String model = "sig Point {} \n" + "\n" + "run { #Point > 1 } for 3 but 3 Int";
    private static String outputfilename = "/tmp/myissue.xml";
    // public static final int NUM_INSTANCES = 5;

    public static void run(String filename, int numInstances, String[] targets) throws Exception {
        A4Reporter rep = new A4Reporter();
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
            while (sol.satisfiable() && i < numInstances) {
                // System.out.println("[Solution]:");
                // System.out.println(sol.toString());
                // sol.writeXML("bijection.xml");
                for(String f : targets) {
                    Expr e = CompUtil.parseOneExpression_fromString(world, f);
                    System.out.println(sol.eval(e));
                }
                sol = sol.next();
                i++;
            }
        }
    }

    public static void main(String[] args) throws Exception {
        if(args.length < 3) {
            System.out.println("usage: <model-filename> <number-of-instances> <list-of-target-functions>");
            System.exit(-1);
        }
        String[] targets = Arrays.copyOfRange(args, 2, args.length);
        run(args[0], Integer.parseInt(args[1]), targets); } }
