import { Queue } from "@datastructures-js/queue";
import { Reference } from "@typescript-eslint/scope-manager";
import {
  AST_NODE_TYPES,
  parse,
  simpleTraverse,
} from "@typescript-eslint/typescript-estree";
import * as constraints from "contrib/Constraints";
import * as functions from "contrib/Functions";
import * as objectives from "contrib/Objectives";
import * as autodiff from "engine/AutodiffFunctions";
import * as im from "immutable";

// !!!
// This basic intraprocedural analysis does not support recursion and does not track shape
// properties through arrays, objects, function calls, or loop iterations.
export const analyzeTsFn = (
  fnName: string,
  arg: string | number
): im.Set<string> => {
  const fieldsUsed: Set<string> = new Set();
  const refQueue: Queue<Reference> = new Queue();

  const src = getTsFnSource(fnName);

  console.log(`Source: ${src}`); // !!!

  // Parse and analyze the function
  const ast = parse(src, {
    range: true,
  });

  console.log(`AST: ${JSON.stringify(ast, null, 2)}`); // !!!

  // Retrieve input parameter name from function, if request was positional
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
    throw new Error(`Unable to find arg ${arg} in fn ${fnName}`);

  console.log(`arg=${argName}`);

  let instances = 0;

  const varQueue: Queue<string> = new Queue([argName]);
  while (!varQueue.isEmpty()) {
    const thisVar = varQueue.dequeue();
    simpleTraverse(ast, {
      enter: (child, parent) => {
        if (
          child.type === AST_NODE_TYPES.Identifier &&
          child.name === thisVar
        ) {
          const node = parent ? parent : child;

          instances++;

          switch (node.type) {
            // !!!
            case AST_NODE_TYPES.MemberExpression:
              if (node.property.type === AST_NODE_TYPES.Identifier)
                fieldsUsed.add(`${argName}.${node.property.name}`);
              break;

            // !!!
            case AST_NODE_TYPES.VariableDeclarator:
              if (
                node.init !== null &&
                node.init.type === AST_NODE_TYPES.Identifier &&
                node.id.type === AST_NODE_TYPES.Identifier &&
                node.id.name !== thisVar
              )
                varQueue.enqueue(node.id.name);
              break;

            // !!!
            case AST_NODE_TYPES.FunctionExpression:
              break;

            // !!!
            case AST_NODE_TYPES.CallExpression: {
              // K-cache / Recursion !!!
              // Local functions !!!
              const callee = node.callee;
              if (
                callee.type === AST_NODE_TYPES.Identifier &&
                parent !== undefined
              ) {
                //try {
                console.log(
                  `Setup analysis for fn ${
                    callee.name
                  } w/arg at position ${node.arguments.indexOf(child)}`
                ); // !!!
                //getTsFnSource(callee.name);
                analyzeTsFn(
                  callee.name,
                  node.arguments.indexOf(child)
                ).forEach((e) => fieldsUsed.add(e));
                //} catch (e: any) {
                //  console.log(
                //    `Error analyzing ${callee.name}; skipping inter-procedural analysis (error: ${e.message})`
                //  );
                //}
              }
              break;
            }

            // !!!
            default:
              throw new Error(
                `Analysis for AST node ${node.type} not yet implemented`
              );
          }
        }
      },
    });
  }
  console.log(
    `RefQueue: ${refQueue.size()}, TraverseInstances: ${instances}, FieldsUsed: ${
      fieldsUsed.size
    }`
  ); // !!!

  // Return immutable set of fields used as input
  return im.Set(fieldsUsed);
};

// !!!
const getTsFnSource = (fnName: string): string => {
  const fnPath = fnName.split(".");
  // Call by dictionary
  if (fnPath[0] === "compDict" && fnPath[1] in functions.compDict)
    return "const f = " + functions.compDict[fnPath[1]].toString();
  else if (fnPath[0] === "objDict" && fnPath[1] in objectives.objDict)
    return "const f = " + objectives.objDict[fnPath[1]].toString();
  else if (fnPath[0] === "constrDict" && fnPath[1] in constraints.constrDict)
    return "const f = " + constraints.constrDict[fnPath[1]].toString();
  // Unresolved call - look in moduiles
  else if (fnPath[0] in autodiff)
    return "const f = " + autodiff[fnPath[0]].toString();
  else if (fnPath[0] in functions)
    return "const f = " + functions[fnPath[0]].toString();
  else if (fnPath[0] in objectives)
    return "const f = " + objectives[fnPath[0]].toString();
  else if (fnPath[0] in constraints)
    return "const f = " + constraints[fnPath[0]].toString();
  // Unresolved call - look in "the secret place" ...
  else if (fnPath[0] in functions.exportsForTestingOrAnalysis)
    return (
      "const f = " + functions.exportsForTestingOrAnalysis[fnPath[0]].toString()
    );
  else throw new Error(`Unable to find fnName: ${fnName}`);
};
