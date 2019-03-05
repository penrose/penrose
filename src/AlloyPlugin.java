import java.io.File;
import java.util.Scanner;
import java.util.Random;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.LinkedHashSet;

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


public class AlloyPlugin {

    private Random rnd;
    private StringBuffer alloyProg;
    private ArrayList<String> facts;

    public AlloyPlugin() {
        alloyProg = new StringBuffer();
        facts     = new ArrayList<String>();
    }

    public String toString() {
        StringBuffer factString = new StringBuffer("fact {\n");
        for(String s : facts)
            factString.append("\t" + s);
        factString.append("}\n");
        return this.alloyProg.toString() + "\n" + factString;
    }

    public void mkFunction(String f, String domain, String codomain) {
        // sig A { f : B }
        String sig = "sig " + domain + " { " + f + " : " + codomain + " }\n";
        this.alloyProg.append(sig);
    }

    public void mkSurjection(String f, String domain, String codomain) {
        // all b : B | some a : A | a.f = b
        String fact = "all b : " + codomain + " | some a : " + domain + " | a." + f + " = b\n";
        this.facts.add(fact);
    }

    // Printing solutions randomly
    public void printSols(ArrayList<String> sols, int numInstances) {
        for(int i = 0; i < numInstances; i++) {
            int index = rnd.nextInt(sols.size());
            System.out.println(sols.get(index));
            sols.remove(index);
        }
    }

    // Main function to run Alloy Analyzer
    public ArrayList<String> run(String filename,  String[] targets) throws Exception {
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
        AlloyPlugin a = new AlloyPlugin();
        a.mkFunction("f", "a", "b");
        a.mkSurjection("f", "a", "b");
        System.out.println(a);

        // if(args.length < 3) {
        //     System.out.println("usage: <model-filename> <number-of-instances> <list-of-target-functions>");
        //     System.exit(-1);
        // }
        // rnd = new Random(System.currentTimeMillis());
        // String[] targets = Arrays.copyOfRange(args, 2, args.length);
        // printSols( run(args[0],  targets), Integer.parseInt(args[1]) );
    }
}

//
// AST version of this thing
// private Set<Sig> sigs;
// private Expr fact;
//
// public AlloyPlugin() {
//     sigs = new LinkedHashSet<Sig>();
//     fact = ExprConstant.TRUE;
// }
//
// public void mkFunction(String f, String domain, String codomain) {
//     // sig A { f : B }
//     PrimSig a = new PrimSig(domain);
//     PrimSig b = new PrimSig(codomain);
//     a.addField(f, b);
//     this.sigs.add(a);
//     this.sigs.add(b);
// }
//
// public void mkSurjection(String f, String domain, String codomain) {
//     // all b : C | some a : B | a.g = b
//     PrimSig a = sigs.get(domain);
//     PrimSig b = sigs.get(codomain);
//     this.forAll()
// }
//
