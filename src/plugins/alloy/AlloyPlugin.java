import java.io.File;
import java.util.Scanner;
import java.util.Random;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.io.*;
import org.json.*;


import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.XMLNode;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Tuple;
import edu.mit.csail.sdg.alloy4compiler.translator.A4TupleSet;
import edu.mit.csail.sdg.alloy4compiler.translator.A4SolutionReader;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;


/**
 * @author: Wode "Nimo" Ni
 * @version: 02/20/2019
 * NOTE: to bulld the plugin, run `make`; to run it, run `java -cp ".:alloy4.2.jar" AlloyPlugin <input-file-name>`
 * NOTE: this plugin requires Java 1.6. On Mac OS Sierra and above, please install "https://support.apple.com/kb/DL1572?locale=en_US"
 */

public class AlloyPlugin {

    private Random rnd;
    private StringBuffer alloyProg;
    private ArrayList<String> facts;
    private int numInstances; // number of total instances generated to be picked at random
    private int maxPts;
    private Map<String, String[]> functions;
    private List<String> surjections, injections, bijections;

    public AlloyPlugin(int numInstances, int maxPts) {
        this.alloyProg    = new StringBuffer();
        this.facts        = new ArrayList<String>();
        this.rnd          = new Random(System.currentTimeMillis());
        this.numInstances = numInstances;
        this.maxPts       = maxPts;
        this.functions    = new HashMap<String, String[]>();
        this.surjections  = new ArrayList<String>();
        this.injections   = new ArrayList<String>();
        this.bijections   = new ArrayList<String>();
    }

    public String mkInjection(String f, String domain) {
        // all a1,a2 : A | a1.f = a2.f implies a1 = a2
        return "all a1, a2: " + domain + " | a1." + f + " = a2." + f + " implies a1 = a2\n";
    }
    public String mkSurjection(String f, String domain, String codomain) {
        // all b : B | some a : A | a.f = b
        return "all b : " + codomain + " | some a : " + domain + " | a." + f + " = b\n";
    }

    public void genFacts() {
        for(String f : surjections) {
            String domain, codomain;
            String[] args = functions.get(f);
            domain = args[0]; codomain = args[1];
            this.facts.add(mkSurjection(f, domain, codomain));
        }
        for(String f : injections) {
            String domain, codomain;
            String[] args = functions.get(f);
            domain = args[0];
            this.facts.add(mkInjection(f, domain));
        }
        for(String f : bijections) {
            String domain, codomain;
            String[] args = functions.get(f);
            domain = args[0]; codomain = args[1];
            // both injective and surjective
            this.facts.add(mkSurjection(f, domain, codomain));
            this.facts.add(mkInjection(f, domain));
        }
    }

    public String getAlloyProg() {

        StringBuffer factString = new StringBuffer("fact {\n");
        for(String s : facts)
            factString.append("    " + s);
        factString.append("}\n");
        factString.append("pred show() { }\n");
        factString.append("run show for " + this.maxPts + "\n");
        return this.alloyProg.toString() + "\n" + factString;
    }

    public void mkFunction(String f, String domain, String codomain) {
        // sig A { f : B }
        String sig1 = "sig " + codomain + " {  }\n";
        String sig2 = "sig " + domain + " { " + f + " : " + codomain + " }\n";
        this.alloyProg.append(sig1);
        this.alloyProg.append(sig2);
        String[] args = {domain, codomain};
        functions.put(f, args);
    }

    // Printing solutions randomly
    public void printSols(ArrayList<String> sols) {
        for(int i = 0; i < this.numInstances; i++) {
            int index = rnd.nextInt(sols.size());
            System.out.println(sols.get(index));
            sols.remove(index);
        }
    }

    // util function for reading a file into a string
    static String readFile(String path) throws IOException {
        InputStream is = new FileInputStream(path);
        BufferedReader buf = new BufferedReader(new InputStreamReader(is));

        String line = buf.readLine();
        StringBuilder sb = new StringBuilder();

        while(line != null){
            sb.append(line).append("\n");
            line = buf.readLine();
        }

        return sb.toString();
    }

    public String translateSet(String set, Module world, A4Solution sol) throws Exception {
        Expr e = CompUtil.parseOneExpression_fromString(world, set);
        A4TupleSet tups = (A4TupleSet) sol.eval(e);
        String res = "";

        for(A4Tuple t : tups) {
            String id = t.atom(0).replace('$', '_');
            res += "Point " + id + "\n";
            res += "PointIn(" + set + ", " + id + ")\n";
        }
        return res;
    }

    // Main function to run Alloy Analyzer
    public ArrayList<String> run() throws Exception {
        A4Reporter rep = new A4Reporter();
        ArrayList<String> solStrings = new ArrayList<String>();

        String tempFilename = "__temp.als__";
        PrintWriter out = new PrintWriter(tempFilename);
        out.println(this.getAlloyProg());
        out.close();

        Module world = CompUtil.parseEverything_fromFile(rep, null, tempFilename);

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
            while (sol.satisfiable()) {
                // System.out.println("[Solution]:");
                // System.out.println(sol.toString());
                // sol.writeXML("bijection.xml");
                String curSolStr = "";
                List<String> targets = new ArrayList<String>();
                targets.addAll(surjections);
                targets.addAll(injections);
                targets.addAll(bijections);
                for(String f : targets) {
                    String[] sets = this.functions.get(f);
                    String domain = sets[0]; String codomain = sets[1];
                    curSolStr += translateSet(domain, world, sol);
                    curSolStr += translateSet(codomain, world, sol);
                    Expr e = CompUtil.parseOneExpression_fromString(world, f);
                    //  If this solution is solved and satisfiable, evaluates the
                    //  given expression and returns an A4TupleSet, a java Integer, or a java Boolean.
                    A4TupleSet tups     = (A4TupleSet) sol.eval(e);
                    for(A4Tuple t : tups) {
                        String p = t.atom(0).replace('$', '_');
                        String q = t.atom(1).replace('$', '_');
                        curSolStr += ("PairIn(" + p + ", " + q + ", " + f + ")\n");
                    }
                }
                solStrings.add(curSolStr);
                sol = sol.next();
                i++;
            }
        }
        return solStrings;
    }

    public void processJSON(JSONObject json) {
        // process predicates
        for(Object o : json.getJSONObject("constraints").getJSONArray("predicates")) {
            JSONObject obj = (JSONObject) o;
            JSONArray arr = obj.getJSONArray("pargNames");
            if(obj.getString("pname").equals("From")) {
                this.mkFunction(arr.getString(0), arr.getString(1), arr.getString(2));
            } else if(obj.getString("pname").equals("Surjection")) {
                this.surjections.add(arr.getString(0));
            } else if(obj.getString("pname").equals("Injection")) {
                this.injections.add(arr.getString(0));
            } else if(obj.getString("pname").equals("Bijection")) {
                this.bijections.add(arr.getString(0));
            }
        }

        // Post-processing
        this.genFacts();
    }

    public static void main(String[] args) throws Exception {
        // check args
        // if(args.length != 1) {
        //     System.out.println("usage: <input-filename>");
        //     System.exit(-1);
        // }

        // read input file
        // String input = readFile(args[0]);
        String input = readFile("Sub_enduser.json");
        System.out.println("Loaded JSON input: ");
        System.out.println(input);
        JSONObject json = new JSONObject(input);

        int numSamples = 10; int pts = 5;
        AlloyPlugin a = new AlloyPlugin(numSamples, pts);

        // processJSON
        a.processJSON(json);
        System.out.println(a.getAlloyProg());

        // write result to file
        List<String> res = a.run();
        int index = a.rnd.nextInt(res.size());
        String output = res.get(index);
        System.out.println("Output from Alloy, translated to Substance: ");
        System.out.println(output);
        PrintWriter out = new PrintWriter("Sub_instantiated.sub");
        out.println(output);
        out.close();

        // HACK: the backend requires values.json, consider removal
        out = new PrintWriter("values.json");
        out.println("[ ]");
        out.close();

        // DEBUG
        // AlloyPlugin a = new AlloyPlugin(numSamples);
        //
        // a.mkFunction("f", "A", "B");
        // a.mkSurjection("f", "A", "B");
        // System.out.println(a);
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
