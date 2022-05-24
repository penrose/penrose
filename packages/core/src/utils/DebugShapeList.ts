import { constrDict } from "contrib/Constraints";
import { compDict } from "contrib/Functions";
import { objDict } from "contrib/Objectives";
import { A } from "types/ast";
import { Shape } from "types/shape";
import { State } from "types/state";
import { FieldPath } from "types/style";
import { FieldExpr, StrV, TrMap, Value } from "types/value";

/**
 * Accepts a diagram state as input and builds a list of shapes, including
 * output values and function applications.  Function application semantcis
 * are goverened by the rules defined in debugFunctionRegistry.
 *
 * @param state Diagram State
 * @returns DebugShapeList
 */
export const buildDebugShapeList = (state: State): DebugShapeList => {
  const translation: TrMap<A> = state.translation.trMap; // Ref. to translation map in diagram state
  const shapes: Shape[] = state.shapes; // Ref. to Shapes in diagram state
  const shapeList: DebugShapeList = {}; // List of Shapes to output
  const compList: FieldExpr<A>[] = []; // List of computations to process
  const transitiveList: {
    comp: FieldExpr<A>;
    done: boolean;
    incoming: Shape;
    outgoing: Shape[];
    fnName: string;
    fnDef: DebugFunctionDef;
  }[] = []; // List of transitive functions (post-process)
  const cascadeList: {
    comp: FieldExpr<A>;
    done: boolean;
    incoming: Shape;
    outgoing: Shape[];
    fnName: string;
    fnDef: DebugFunctionDef;
  }[] = []; // List of computations to cascade (post-process)

  // Build the data structure using the translation objects as input
  for (const obj in translation) {
    console.log(`Reading ${obj}`); // !!!

    // Defer processing of computation nodes: they might
    // refer to objects we have not yet encountered in the translation.
    if (obj.charAt(0) === "$") {
      compList.push(...Object.values(translation[obj]));
      continue;
    }

    // Loop over the object's fields
    for (const field in translation[obj]) {
      console.log(` - Reading ${field}`); // !!!

      // Skip non-GPIs
      if (translation[obj][field].tag !== "FGPI") continue;

      // Copy the Shape from the Shape State.  We copy from shapes instead of
      // from the translation due to all references, etc. being resolved.
      let found = false;
      for (const shape in shapes) {
        if (shapes[shape].properties.name.contents === `${obj}.${field}`) {
          found = true;
          if (!(obj in shapeList)) shapeList[obj] = {};
          shapeList[obj][field] = shapes[shape];
          break;
        }
      } // for: shapes

      // Raise an error if we did not find a matching shape
      if (!found) {
        throw new Error(
          `Shape ${obj}.${field} present in translation but not in shapes`
        );
      }
    } // for: fields
  } // for: objects

  // Process the computations to derive any additional properties
  while (compList.length) {
    const thisComp: FieldExpr<A> | undefined = compList.pop();

    // Narrow down our types to aid type checking.  TODO: Add support for layering
    if (
      !(
        thisComp &&
        thisComp.tag === "FExpr" &&
        thisComp.contents.tag === "OptEval" &&
        (thisComp.contents.contents.tag === "ObjFn" ||
          thisComp.contents.contents.tag === "ConstrFn") &&
        thisComp.contents.contents.name.value in debugFunctionRegistry
      )
    ) {
      console.log(`Skipping comp: ${JSON.stringify(thisComp)}`); // !!!
      continue;
    }

    const fnName = thisComp.contents.contents.name.value; // Function name
    const fnDef = debugFunctionRegistry[fnName]; // Function definition in debugger
    const fnPropName = fnName + "[]"; // Name of the function property
    const fnShapeArgs: Shape[] = []; // Shape arguments to the computation

    // Accumulate all the shape arguments
    for (const i in thisComp.contents.contents.args) {
      const thisArg = thisComp.contents.contents.args[i];
      if (thisArg.tag === "FieldPath") {
        fnShapeArgs.push(resolveFieldPath(thisArg, shapeList));
      }
    } // for: function shape arguments

    console.log(`${fnName} has ${fnShapeArgs.length} shape args`); // !!!

    // If there are no arguments, skip the computation
    if (!fnShapeArgs.length) continue;

    // Bidirectional Mapping
    // ---------------------
    // Create properties for each shape argument to represent the functions
    // classified as 'bi-directional' that apply equally to all shapes
    // provided as arguments.  For instance, near(A, B) applies equally to
    // both A and B and thus A.near[] contains B and B.near[] contains A.
    if (fnDef.bidi) {
      for (const shape in fnShapeArgs) {
        // Leave off the current shape: A.near[] does not need to contain A
        const argsMinusThisShape = fnShapeArgs.filter((shapeArg) => {
          return shapeArg !== fnShapeArgs[shape];
        });
        const shapeProperties = fnShapeArgs[shape].properties;
        const fnProp: Value<number> | undefined =
          fnPropName in shapeProperties
            ? shapeProperties[fnPropName]
            : undefined;

        // Add the shape(s) to the property
        if (fnProp && fnProp.tag === "StrV") {
          shapeProperties[fnPropName] = addToShapePropertyList(
            argsMinusThisShape,
            fnProp
          );
        } else {
          shapeProperties[fnPropName] = addToShapePropertyList(
            argsMinusThisShape
          );
        }
        console.log(
          `Set Bidi ${JSON.stringify(
            shapeProperties.name.contents
          )}.${fnPropName} = ${JSON.stringify(
            fnShapeArgs[shape].properties[fnPropName].contents
          )}` // !!!
        );
      } // for: fnShapeArgs
    } else {
      // Unidirectional Mapping (default)
      // --------------------------------
      // Creates one property on this shape to represent the function and its other
      // shape arguments. This shape is the first argument. For example, contains(A,B)
      // is unidirectional: A contains B but B does not contain A.
      if (fnShapeArgs.length) {
        const shape = fnShapeArgs.shift(); // Take the first shape off the argument list
        if (shape === undefined) continue; // Happify tsc

        const fnProp: Value<number> | undefined =
          fnPropName in shape.properties
            ? shape.properties[fnPropName]
            : undefined;

        // Add the shapes to the property
        if (fnProp && fnProp.tag === "StrV") {
          shape.properties[fnPropName] = addToShapePropertyList(
            fnShapeArgs,
            fnProp
          );
        } else {
          shape.properties[fnPropName] = addToShapePropertyList(fnShapeArgs);
        }
        console.log(
          `Set UniDi ${JSON.stringify(
            shape.properties.name.contents
          )}.${fnPropName} = ${JSON.stringify(
            shape.properties[fnPropName].contents
          )}`
        ); // !!!

        // Defer transitives
        if (fnDef.transitive) {
          // !!!
        }

        // Defer cascades until after we process all the uni-directional properties.
        cascadeList.push({
          comp: thisComp,
          done: false,
          incoming: shape,
          outgoing: fnShapeArgs,
          fnName: fnName,
          fnDef: fnDef,
        });
      }
    } // else: uni-directional
  } // while: computations to process

  // Transitive properties
  // ---------------------
  // Propogate properties such as contains() that are transitive.  For example, if
  // A.contains(B) and B.contains(C) then A.contains(C) is also true.

  // !!!

  // Cascading properties
  // --------------------
  // Loop over the list of cascades we deferred previously.  Due to cascading, multiple
  // iterations may be needed to stabalize all properties. Here we maintain a worklist of
  // items that need to be processed.  When an item does not change, it is marked as done.
  // If an item changes, any outbound edges are marked as not done and are worked in the
  // next cycle.
  // TODO: Check for cycles !!!
  let workRemains = true;
  while (workRemains) {
    console.log("---------Cascade Pass Begins---------"); // !!!
    workRemains = false;

    for (const i in cascadeList) {
      console.log(`cascadeList[${i}]`); // !!!
      const thisCascade = cascadeList[i];
      let changesMade = false;

      if (thisCascade.done) continue; // Skip complete work items

      for (const cascadeFn in thisCascade.fnDef.cascadeUp) {
        const fnPropName = thisCascade.fnDef.cascadeUp[cascadeFn] + "[]";
        const fnProperties = thisCascade.incoming.properties;
        console.log(` - cascadeFn: ${fnPropName}`); // !!!

        if (
          !(
            fnPropName in fnProperties &&
            fnProperties[fnPropName].tag === "StrV"
          )
        ) {
          console.log(`   - Skipping`);
          continue;
        }

        for (const j in thisCascade.outgoing) {
          console.log(
            `    - shape ${JSON.stringify(
              thisCascade.outgoing[j].properties.name
            )}`
          ); // !!!
          const outShape = thisCascade.outgoing[j];
          const inProperty = fnProperties[fnPropName];
          let oldPropString = "";
          let newPropString = "";
          const newPropValue =
            inProperty.tag === "StrV"
              ? new Set(inProperty.contents.split(";"))
              : new Set();

          // Get the property value of the outgoing shape -- if it exists
          const outProperty: Value<number> | undefined =
            fnPropName in outShape.properties
              ? outShape.properties[fnPropName]
              : undefined;

          // Merge the two property values
          if (outProperty && outProperty.tag === "StrV") {
            oldPropString = outProperty.contents;
            outProperty.contents.split(";").forEach((e) => {
              newPropValue.add(e);
            });
            newPropString = [...newPropValue].join(";");
            outShape.properties[fnPropName] = {
              tag: "StrV",
              contents: newPropString,
            };

            console.log(
              `      - Set Cascade ${JSON.stringify(
                outShape.properties.name.contents
              )}.${fnPropName} = ${JSON.stringify(
                outShape.properties[fnPropName].contents
              )}`
            ); // !!!
            //console.log(`    Was: ${oldPropString}`); // !!!
            //console.log(`    Now: ${newPropString}`); // !!!
          } else {
            newPropString = [...newPropValue].join(";");
            outShape.properties[fnPropName] = {
              tag: "StrV",
              contents: newPropString,
            };
            console.log(
              `Set Cascade ${JSON.stringify(
                outShape.properties.name.contents
              )}.${fnPropName} = ${JSON.stringify(
                outShape.properties[fnPropName].contents
              )}`
            ); // !!!
          }

          // Indicate work was performed and propogate work along the outbound edges
          if (oldPropString !== newPropString) {
            // Indicate work was done (so we don't stop)
            changesMade = true;

            // Improve performance here !!!
            for (const k in cascadeList) {
              if (cascadeList[k].incoming === outShape)
                cascadeList[k].done = false;
            }
          }
        } // for: outgoing shapes
      } // for: each cascaded function

      // If we are at a fixed point, mark this work item done.  If a lower set of
      // work makes a relevant change, it will re-activate it upon any changes.
      if (!changesMade) {
        thisCascade.done = true;
      }
    } // for: cascaded computations

    // Determine if work remains.  Improve performance here !!!
    workRemains = false;
    for (const i in cascadeList) {
      console.log(` - ${i} done status: ${cascadeList[i].done}`); // !!!
      if (!cascadeList[i].done) {
        workRemains = true;
        console.log(`${i} is still active - workRemains = true`); // !!!
        break;
      }
    }
  } // while: work exists

  return shapeList;
};

/**
 * This function registry contains the rules for how to apply functions to
 * shapes in the DebugShapeList.  See DebugFunctionDef for details about
 * each directive.  Note: A test checks that this registry is consistent
 * with the core function libraries.
 */
export const debugFunctionRegistry: {
  [k: string]: DebugFunctionDef;
} = {
  // ----------------------Functions that Require Processing---------------//

  repelPt: {
    contradicts: ["nearPt"],
  },
  above: {
    contradicts: ["below", "contains", "sameCenter"],
  },
  below: {
    contradicts: ["above", "contains", "sameCenter"],
  },
  leftwards: {
    contradicts: ["rightwards", "contains", "sameCenter"],
  },
  rightwards: {
    contradicts: ["leftwards", "contains", "sameCenter"],
  },
  sameCenter: {
    contradicts: [
      "above",
      "below",
      "leftwards",
      "rightwards",
      "disjoint",
      "repel",
    ],
    bidi: true,
  },
  repel: {
    contradicts: ["near", "sameCenter", "contains", "overlapping", "touching"],
    bidi: true,
  },
  near: {
    contradicts: ["repel"],
    bidi: true,
  },
  nearPt: {
    contradicts: ["repelPt"],
  },
  touching: {
    contradicts: ["repel", "disjoint"],
    bidi: true,
  },
  overlapping: {
    contradicts: ["repel"],
    bidi: true,
  },
  disjoint: {
    contradicts: ["contains", "sameCenter", "touching"],
    bidi: true,
  },
  contains: {
    contradicts: [
      "disjoint",
      "above",
      "below",
      "rightwards",
      "leftwards",
      "repel",
    ],
    cascadeUp: [
      "disjoint",
      "onCanvas",
      "smallerThan",
      "maxSize",
      "above",
      "below",
      "leftwards",
      "rightwards",
      "repel",
      "near",
      "nearPt",
      "smallerThan",
    ],
    transitive: true,
  },
};

/**
 * Adds an array of shapes to an existing property list
 *
 * @param shapesToAdd Array of Shape objects to add
 * @param shapeProp Existing StrV shape property
 * @returns new StrV
 */
const addToShapePropertyList = (
  shapesToAdd: Shape[],
  shapeProp?: StrV
): StrV => {
  const outShapeProp: StrV = {
    tag: "StrV",
    contents: "",
  };

  // First, get the existing property value, which is a list of shapes
  const propertyShapes: Set<string> = new Set();
  if (shapeProp !== undefined) {
    shapeProp.contents.split(";").forEach((shapeName) => {
      propertyShapes.add(shapeName);
    });
  }

  // Add the new values
  shapesToAdd.forEach((shape) => {
    if (shape.properties.name.tag === "StrV") {
      propertyShapes.add(shape.properties.name.contents);
    }
  });

  // Update & return the property
  outShapeProp.contents = [...propertyShapes].join(";");
  return outShapeProp;
};

/**
 * Resolves a field path to a shape
 *
 * @param fieldPath Path to Shape
 * @param shapeList List of Shapes
 * @returns Shape that corresponds to fieldPath
 */
const resolveFieldPath = (
  fieldPath: FieldPath<unknown>,
  shapeList: DebugShapeList
): Shape => {
  const objName = fieldPath.name.contents.value;
  const fieldName = fieldPath.field.value;
  if (objName in shapeList && fieldName in shapeList[objName]) {
    return shapeList[objName][fieldName];
  } else {
    throw new Error(
      `Unable to field fieldPath ${objName}.${fieldName} in ShapeList`
    );
  }
};

/**
 * Compares the debugFunctionRegistry to the main three function dictionaries
 * to ensure (1) functions in both sets completely overlap and (2) the internal
 * configuration within debugFunctionRegistry is consistent.
 *
 * Note: This check is intended to be called by the test suite during CI to
 * ensure the registries are consistent.  It is not intended to be called during
 * runtime.
 *
 * @returns string[] of inconsistency messages
 */
export const getInconsistentDebugFunctions = (): string[] => {
  const errorList: string[] = [];

  for (const fn in debugFunctionRegistry) {
    const fnDef = debugFunctionRegistry[fn];

    // Verify the function exists
    if (!(fn in compDict || fn in objDict || fn in constrDict)) {
      errorList.push(
        `Inconsistency in debugFunctionRegistry: Fn not found in compDict, objDict, or constrDict: ${fn}`
      );
    }

    // Verify that no bi-directional function is also cascading up (not supported yet)
    if (fnDef.bidi && fnDef.cascadeUp) {
      errorList.push(
        `Inconsistency in debugFunctionRegistry: Fn cannot be both bidi and cascadeUp: ${fn}`
      );
    }

    // Verify all contradiction functions exist
    if ("contradicts" in fnDef) {
      for (const contraFn in fnDef.contradicts) {
        const contraFnName = fnDef.contradicts[contraFn];

        // Check that the function exists
        if (
          !(
            contraFnName in compDict ||
            contraFnName in objDict ||
            contraFnName in constrDict
          )
        ) {
          errorList.push(
            `Inconsistency in debugFunctionRegistry: contraFn not found: ${contraFnName}`
          );
        }

        // Check that any contradiction is bi-directional
        const contraFnDef = debugFunctionRegistry[contraFnName];
        if (
          !(
            contraFnName in debugFunctionRegistry &&
            contraFnDef.contradicts !== undefined &&
            contraFnDef.contradicts.includes(fn)
          )
        ) {
          errorList.push(
            `Inconsistency in debugFunctionRegistry: Fn '${fn}' contradicts '${contraFnName}', but '${contraFnName}' does not contradict '${fn}'`
          );
        }
      }
    } // if: contra Fn

    // Verify cascadeUp functions exist
    if ("cascadeUp" in fnDef) {
      for (const cascadeFn in fnDef.cascadeUp) {
        const cascadeFnName = fnDef.cascadeUp[cascadeFn];
        if (
          !(
            cascadeFnName in compDict ||
            cascadeFnName in objDict ||
            cascadeFnName in constrDict
          )
        ) {
          errorList.push(
            `Inconsistency in debugFunctionRegistry: cascadeUp Fn not found: ${cascadeFnName}`
          );
        }
      } // for: fnDef.cascadeUp
    } // if: cascadeUp Fn

    // Check that all functions in the dictionaries are in the debug registry
    /*
    for (const compFn in compDict) {
      if (!(compFn in debugFunctionRegistry)) {
        errorList.push(
          `Inconsistency in debugFunctionRegistry: '${compFn} found in compDict but not in debugFunctionRegistry`
        );
      }
    }
    for (const objFn in objDict) {
      if (!(objFn in debugFunctionRegistry)) {
        errorList.push(
          `Inconsistency in debugFunctionRegistry: '${objFn} found in objDict but not in debugFunctionRegistry`
        );
      }
    }
    for (const constrFn in constrDict) {
      if (!(constrFn in debugFunctionRegistry)) {
        errorList.push(
          `Inconsistency in debugFunctionRegistry: '${constrFn} found in constrDict but not in debugFunctionRegistry`
        );
      }
    }
    */
  }
  return errorList;
};

/**
 * A list of shapes organized in three levels:
 *   (1) object,
 *   (2) field,
 *   (3) property
 */
export type DebugShapeList = Record<string, Record<string, Shape>>;

/**
 * A Debug Function Definition that configures how functions are applied to
 * shapes in the DebugShapeList.  These defintions do not control optimization
 * or constraint rendering; instead, they represent an abstraction of some of
 * the complex relationships among functions and shapes.
 */
type DebugFunctionDef = {
  // contradicts:
  // ------------
  // List of functions this function contradicts.  For example, disjoint()
  // contradicts contains().
  contradicts?: string[];

  // cascadeUp:
  // ----------
  // List of functions, if present on the shape this function is applied to,
  // to copy/merge with this function's shape arguments.  For example, if
  // A.contains(B) and A.disjoing(C), then B.disjoint(C) should also apply.
  //
  // Note: bidi and cascade are mututally exclusive
  cascadeUp?: string[];

  // transitive:
  // ------------
  // Indicates the function is transitive. For example, if A.contains(B) and
  // B.contains(C), then A.contains(C) should also be true.
  transitive?: boolean;

  // bidi: (Bi-directional)
  // ----------------------
  // Bi-directional functions affect every shape in in the function argument list
  // and are applied similarly to each shape in DebugShapeList.  For example, near(A,B)
  // generates properties for both A.near(B) and B.near(A).
  //
  // If bidi is false or not present, the default behavior is used, which applies the
  // function to the first shape argument.
  //
  // Note: bidi and cascade are mututally exclusive
  bidi?: boolean;
};
