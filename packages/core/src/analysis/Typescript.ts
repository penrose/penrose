import { Queue } from "@datastructures-js/queue";
import {
  AST_NODE_TYPES,
  parse,
  simpleTraverse,
  TSESTree,
} from "@typescript-eslint/typescript-estree";
import * as constraints from "contrib/Constraints";
import * as functions from "contrib/Functions";
import * as objectives from "contrib/Objectives";
import * as utils from "contrib/Utils";
import { ops } from "engine/Autodiff";
import * as autodiff from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import * as im from "immutable";
import * as _ from "lodash";
import { noop } from "lodash";
import * as util from "utils/Util";

/**
 * Simple inter-procedural analysis of property accesses for a given typescript function name
 * and one of its arguments. If multiple arguments need to be analyzed, call this function
 * onces for each argument.
 *
 * Limitations on Soundness and Other Important Notes:
 *  - This analysis is not complete: it does not provide all true answers.
 *  - Arg is not tracked through arrays, objects, loop iterations, or dynamic calls.
 *    E.g., situations such as these are not analyzed and will likely throw an exception:
 *     - x = [s]; y = x[0]  <-- s is not tracked through its assignment into the array
 *     - x = {s:s}; y = x.s <-- s is not tracked through its assignment into an object
 *  - Multiple assignments (let x=s1;x=s2) are not analyzed and raise an exception.
 *  - Nested scopes within a function are not analyzed and do not raise an exception.
 *  - Functions nested within other functions are not analyzed and raise an exception.
 *    (This is obviously possible but is not implemented yet)
 *
 * These approximations are acceptable for our simple use case.  If our computation or
 * constraint functions begin to use the features above, this analysis will need to
 * become more sophisticated.
 *
 * @param fnName Name of the constraint, objective, or computation function to analyze.  I.e., "compDict.chevron"
 * @param arg Either the name (string) or the position (number) of the fn argument to analyze.
 * @param fnSource Optional source of function (retrieved by calling fn.toString())
 * @returns Immutable Set of property accesses in the format: fn::var.property
 */
export const tsAnalyzePropertyAccess = (
  fnName: string,
  arg: string | number,
  fnSource?: string
): im.Set<TsPropertyAccess> => {
  /**
   * Pending indicator - to avoid infinite recursion during analysis.
   */
  const PENDING = im.Set<string>("PENDING");

  /**
   * Cache of inter-procedural calls - improves performance and avoids infinite recursion
   */
  const interProcedureCache: Record<
    string,
    Record<string, Record<string, Record<number, im.Set<string>>>>
  > = {};

  /**
   * Accesses the cache for an inter-procedural call.  If a hit is not found,
   * then make the inter-procedural call and cache the result.  Our context is
   * determined by caller, callee, AST node position, and arg position.
   *
   * @param callerFn Name of function doing the calling
   * @param calleeFn Name of function being called
   * @param node The AST node where the call takes place
   * @param arg The argument offset
   * @returns Immutable Set containing results of inter-procedural analysis
   */
  const getInterProcedureResult = (
    callerFn: string,
    calleeFn: string,
    node: TSESTree.Node,
    arg: number
  ): im.Set<string> => {
    // Get the string representation of the AST position.
    const nodePos = `${node.range[0]}-${node.range[1]}`;

    // Fill up the cache data structure as needed
    if (!(callerFn in interProcedureCache)) interProcedureCache[callerFn] = {};
    if (!(calleeFn in interProcedureCache[callerFn]))
      interProcedureCache[callerFn][calleeFn] = {};
    if (!(nodePos in interProcedureCache[callerFn][calleeFn]))
      interProcedureCache[callerFn][calleeFn][nodePos] = {};

    // We need to handle three cases:
    //  1. First inter-procedural call for a context:
    //     - set the cache to pending (to detect recursion)
    //     - do the interprocedural call and save the result
    //     - return the call result
    //  2. Another call is already pending for this context (recursion)
    //     - return an empty set
    //  3. Another call previously completed for this context
    //     - return the cached result
    if (!(arg in interProcedureCache[callerFn][calleeFn][nodePos])) {
      interProcedureCache[callerFn][calleeFn][nodePos][arg] = PENDING;
      interProcedureCache[callerFn][calleeFn][nodePos][
        arg
      ] = tsAnalyzePropertyAccessImpl(calleeFn, arg);
    } else if (
      interProcedureCache[callerFn][calleeFn][nodePos][arg] === PENDING
    ) {
      return im.Set();
    }

    // Return the cached value
    return interProcedureCache[callerFn][calleeFn][nodePos][arg];
  }; // fn: getInterProcedureResult

  /**
   * Accepts a callee AST and returns a string function name.
   *
   * @param callee Expression representing the fn to call
   * @returns string of fn to call
   */
  const getCalleeFnName = (callee: TSESTree.Expression): string => {
    // Strip off the call resolution sequence wrapper, if present
    if (
      callee.type === AST_NODE_TYPES.SequenceExpression &&
      callee.expressions[1].type === AST_NODE_TYPES.MemberExpression &&
      callee.expressions[1].property.type === AST_NODE_TYPES.Identifier
    )
      return callee.expressions[1].property.name;

    // Retrieve the function name from the Callee AST subtree
    if (callee.type === AST_NODE_TYPES.Identifier) return callee.name;
    if (callee.type === AST_NODE_TYPES.MemberExpression) {
      if (
        callee.object.type === AST_NODE_TYPES.Identifier &&
        callee.property.type === AST_NODE_TYPES.Identifier
      )
        return `${callee.object.name}.${callee.property.name}`;
      else
        throw new Error(
          `Analysis for AST Callee ${callee.type}::${callee.object.type}.${callee.property.type} not yet implemented`
        );
    }

    // If no calleeName was retrieved, raise an exception
    throw new Error(
      `Analysis for AST Callee ${
        callee.type
      } not yet implemented: ${JSON.stringify(callee, null, 2)}`
    );
  };

  /**
   * Function that implements the intra-procedural analysis
   */
  const tsAnalyzePropertyAccessImpl = (
    fnName: string,
    arg: string | number,
    fnSource?: string
  ): im.Set<string> => {
    const propsAccessed = new Set<string>(); // List of properties accessed
    fnName = fnName.replaceAll(":", ""); // No colons
    fnSource ? (fnSource = "const $_f=" + fnSource) : noop; // Prefix the source

    // Retrieve fn source and parse it
    const src = fnSource || getTsFnSource(fnName);
    const ast = parse(src, { range: true });

    // Log a warning and return if the function was not found
    if (src === "")
      throw new Error(
        `tsAnalyzePropertyAccess unable to find src of fn: ${fnName}`
      );

    // If the arg to analyze is positional (numeric), determine its name
    let argName: string | undefined;
    if (typeof arg === "string") argName = arg;
    else {
      const fnDecl = ast.body[0];
      if (fnDecl.type === AST_NODE_TYPES.VariableDeclaration) {
        const fnInit = fnDecl.declarations[0].init;
        if (
          fnInit !== null &&
          fnInit.type === AST_NODE_TYPES.FunctionExpression
        ) {
          const thisParam = fnInit.params[arg];
          if (thisParam.type === AST_NODE_TYPES.Identifier)
            argName = thisParam.name;
        }
      }
    }
    if (argName === undefined)
      throw new Error(`Unable to find arg at position ${arg} in fn ${fnName}`);

    // Queue of fn vars to analyze for property access
    const varQueue: Queue<string> = new Queue([argName]);
    let queueHits = 0; // Number of nodes we entered

    // Process each local variable to analyze in varQueue
    while (!varQueue.isEmpty()) {
      const thisVar = varQueue.dequeue();

      // Traverse the AST to fine property accesses
      simpleTraverse(ast, {
        enter: (child, parent) => {
          // Find AST nodes where this variable is used
          if (
            child.type === AST_NODE_TYPES.Identifier &&
            child.name === thisVar
          ) {
            // If the parent is provided, use it; otherwise, use the child
            // We do this because it is not very useful to analyze just the
            // identifier node; we need the parent context for this analysis.
            const node = parent ? parent : child;
            queueHits++;

            // Process the parent node type
            switch (node.type) {
              // If a property is accessed, add it to propsAccessed
              case AST_NODE_TYPES.MemberExpression:
                if (node.property.type === AST_NODE_TYPES.Identifier)
                  // We use a string here to easily eliminate dupes
                  propsAccessed.add(
                    `${fnName}::${thisVar}::${node.property.name}`
                  );
                break;

              // If the variable is assigned to another, analyze it too.
              // This is an over-approximation, but it is suitable for
              // our analysis where we want to know which properties are
              // used within a function.
              case AST_NODE_TYPES.VariableDeclarator:
                if (
                  node.init !== null &&
                  node.init.type === AST_NODE_TYPES.Identifier &&
                  node.id.type === AST_NODE_TYPES.Identifier &&
                  node.id.name !== thisVar
                )
                  varQueue.enqueue(node.id.name);
                break;

              // Ignore use in the function signature (i.e., in the
              // inter-procedural analysis situation.)
              case AST_NODE_TYPES.FunctionExpression:
                break;

              // If the variable is a parameter to a function call,
              // perform inter-procedural analysis to analyze the
              // variable's property accesses in the called function.
              case AST_NODE_TYPES.CallExpression: {
                // Perform the inter-procedural analysis and add
                // the property accesses it detected to our own list.
                getInterProcedureResult(
                  fnName,
                  getCalleeFnName(node.callee),
                  node,
                  node.arguments.indexOf(child)
                ).forEach((e) => propsAccessed.add(e));
                break;
              }

              // If we encounter a situation we don't support yet, such as
              // array or object assignments, throw an exception to avoid
              // unsound answers.
              default:
                throw new Error(
                  `Analysis for AST node ${node.type} not yet implemented`
                );
            } // switch: node.type
          } // if: AST node is identifier for this variable
        }, // enter
      }); // traverse AST
    } // while: varQueue not empty

    // Error if we found no use of the requested argument
    if (!queueHits)
      throw new Error(`No use of argument '${argName}' in ${fnName}`);

    // Return the properties accessed
    return im.Set(propsAccessed);
  }; // fn: tsAnalyzePropertyAccessImpl

  // Call the function that implements the analysis
  const propAccesses = tsAnalyzePropertyAccessImpl(fnName, arg, fnSource);

  // Return immutable set of properties accesses using exported type
  // TsPropertyAccess. We type Set<string> in the implementing function as
  // it makes it duplicate elimination fast and easy.
  const propAccessesArr: TsPropertyAccess[] = [];
  propAccesses.forEach((e) => {
    const [fnName, varName, propName] = e.split("::");
    console.log(`${JSON.stringify(e)} => ${JSON.stringify(e.split("::"))}`); // !!!
    propAccessesArr.push({ fnName, varName, propName });
  });

  return im.Set(propAccessesArr);
}; // fn: tsAnalyzePropertyAccess

/**
 * Resolves and retrieves the source code for a constraint, objective,
 * or computation function using a qualified function name such as:
 * "compDict.chevron".
 *
 * If the fn name is not qualified by a known dictionary name, attempt
 * to resolve it using this search strategy: AutodiffFunctions,
 * Functions, Objectives, Constraints, Util, Utils, BBox, lodash, and
 * finally the list of non-exported functions in Functions.
 *
 * @param fnName The name of the function (qualified or unqualified)
 * @returns Source code of the function. "" if not found.
 */
const getTsFnSource = (fn: string): string => {
  const fnPath = fn.split(".");
  const prefix = "const $_f="; // required for parsing

  const fnObject = fnPath.length > 1 ? fnPath[0] : undefined;
  const fnName = fnPath.length > 1 ? fnPath[1] : fnPath[0];

  // Call via dictionary
  if (fnObject === "compDict" && fnName in functions.compDict)
    return prefix + functions.compDict[fnName].toString();
  if (fnObject === "objDict" && fnName in objectives.objDict)
    return prefix + objectives.objDict[fnName].toString();
  if (fnObject === "constrDict" && fnName in constraints.constrDict)
    return prefix + constraints.constrDict[fnName].toString();
  if (fnObject === "ops" && fnName in ops)
    return prefix + ops[fnName].toString();
  if (fnObject === "util" && fnName in util)
    return prefix + util[fnName].toString();
  if (fnObject === "utils" && fnName in utils)
    return prefix + utils[fnName].toString();
  if (fnObject === "BBox" && fnName in BBox)
    return prefix + BBox[fnName].toString();
  if (fnObject === "_" && fnName in _) return prefix + _[fnName].toString();

  // Not a dictionary call - look in moduiles
  if (fnName in autodiff) return prefix + autodiff[fnName].toString();
  if (fnName in functions) return prefix + functions[fnName].toString();
  if (fnName in objectives) return prefix + objectives[fnName].toString();
  if (fnName in constraints) return prefix + constraints[fnName].toString();
  if (fnName in util) return prefix + util[fnName].toString();
  if (fnName in utils) return prefix + utils[fnName].toString();
  if (fnName in BBox) return prefix + BBox[fnName].toString();
  if (fnName in _) return prefix + _[fnName].toString();

  // Still not found ... check non-exported functions ...
  if (fnName in functions.exportsForTestingOrAnalysis)
    return prefix + functions.exportsForTestingOrAnalysis[fnName].toString();

  // Still not found, very sad...
  return "";
}; // fn: getTsFnSource

/**
 * Records an access of propName from variable varName in function fnName
 */
export type TsPropertyAccess = {
  fnName: string;
  varName: string;
  propName: string;
};
