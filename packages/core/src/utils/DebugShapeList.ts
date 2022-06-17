import { Queue } from "@datastructures-js/queue";
import { constrDict } from "contrib/Constraints";
import { compDict } from "contrib/Functions";
import { objDict } from "contrib/Objectives";
import { Shape } from "types/shape";
import { Fn } from "types/state";
import { Path } from "types/style";
import { Subst, Translation } from "types/styleSemantics";

/**
 * A list of shapes organized in three levels:
 *   (1) objects    --> A
 *   (2) fields     --> A.icon
 *   (3) properties --> A.icon.strokeWidth
 */
export type DebugShapeList = Record<string, Record<string, Shape>>;

/**
 * Accepts a diagram state as input and builds a list of shapes, including
 * output values and function applications.  Function application semantics
 * are goverened by the rules defined in debugFunctionRegistry.
 *
 * @param state Diagram State
 * @returns DebugShapeList
 */
export const buildDebugShapeList = (
  translation: Translation
): DebugShapeList => {
  const shapeList: DebugShapeList = {}; // List of Shapes to output
  const fnQueue = new Queue([
    ...translation.constraints,
    ...translation.objectives,
  ]);

  // Build the basic three-layer data structure using the translation objects as input
  translation.symbols
    .filter((e) => e.tag === "GPI")
    .forEach((gpi, gpiName) => {
      const [gpiObj, gpiField] = gpiName.replaceAll("`", "").split(".");

      // Create the object in shapeList if it does not exist yet
      if (!(gpiObj in shapeList)) shapeList[gpiObj] = {};

      // Make a copy of the shape before we manipulate it
      shapeList[gpiObj][gpiField] = {
        shapeType: gpi.contents[0],
        properties: {
          ...gpi.contents[1],
          _shapeName: { tag: "StrV", contents: `${gpiObj}.${gpiField}` },
        },
      };
    }); // for: GPIs

  // Add function properties to shapes.
  addFunctionPropertiesToShapeList(shapeList, fnQueue);

  // Add function properties implied by function transitivity.
  // For example, if A.contains(B) and B.contains(C), then
  // A.contains(C) should also be true.
  addTransitivesToShapeList(shapeList);

  // Add function properties implied by cascading function semantics.
  // For example, if A.contains(B) and A.disjoint(C), then
  // B.disjoint(C) should also be true.
  addCascadesToShapeList(shapeList);

  return shapeList;
};

/**
 * Adds function properties to shapes. For example, contains(A.icon,B.icon)
 * is added as a shape property: A.icon.contains[] = B.icon
 *
 * TODO: Add support for layering
 *
 * @param shapeList
 * @param fnQueue
 */
const addFunctionPropertiesToShapeList = (
  shapeList: DebugShapeList,
  fnQueue: Queue<Fn>
): void => {
  // Process the functions to derive any additional properties
  while (!fnQueue.isEmpty()) {
    const thisFn = fnQueue.dequeue().ast;

    const fnName = thisFn.expr.name.value; // Function name
    const fnDef = getFnDef(fnName); // Function definition in debugger
    const fnPropName = fnName + "[]"; // Name of the function property

    // Accumulate the function's shape arguments
    const fnShapeArgs: Shape[] = [];
    for (const i in thisFn.expr.args) {
      const thisArg = thisFn.expr.args[i];
      if (thisArg.tag === "Path") {
        fnShapeArgs.push(resolvePath(thisArg, shapeList, thisFn.context.subst));
      }
    } // for: function shape arguments

    // If there are no shape arguments, skip the computation
    if (!fnShapeArgs.length) continue;

    if (fnDef.bidi) {
      // Bidirectional Mapping
      // ---------------------
      // Create properties for each shape argument to represent the functions
      // classified as 'bi-directional' that apply equally to all shapes
      // provided as arguments.  For instance, near(A, B) applies equally to
      // both A and B and thus A.near[] contains B and B.near[] contains A.
      for (const i in fnShapeArgs) {
        const thisShapeArg = fnShapeArgs[i];

        // Build a list of arguments excluding this shape:
        // A.near[] does not need to contain A
        const argsMinusThisShape = fnShapeArgs
          .filter((shapeArg) => shapeArg !== thisShapeArg)
          .map((shapeArg) => getShapeName(shapeArg))
          .join(";");

        // Add the shape arguments to property thisShapeArg[fnPropName]
        setStrPropertyValue(thisShapeArg, fnPropName, argsMinusThisShape, true);
      } // for: fnShapeArgs
    } else {
      // Unidirectional Mapping (default)
      // --------------------------------
      // Creates one property on this shape to represent the function and its other
      // shape arguments. This shape is the first argument. For example, contains(A,B)
      // is unidirectional: A contains B but B does not contain A.
      if (fnShapeArgs.length) {
        const thisShape = fnShapeArgs.shift(); // Take the first shape off the argument list
        const fnShapeArgsStr = fnShapeArgs
          .map((shapeArg) => getShapeName(shapeArg))
          .join(";");
        if (thisShape === undefined) continue; // Happify tsc

        // Add the shape arguments to property thisShapeArg[fnPropName]
        setStrPropertyValue(thisShape, fnPropName, fnShapeArgsStr, true);
      }
    } // else: uni-directional
  } // while: computations to process
};

/**
 * Propogate properties such as contains() that are transitive.  For example, if
 * A.contains(B) and B.contains(C) then A.contains(C) is also true.
 *
 * Requires that the shapeList already be populated with shapes and properties.
 *
 * @param shapeList DebugShapeList to add transitives to.
 */
const addTransitivesToShapeList = (shapeList: DebugShapeList): void => {
  // Get the list of transitive functions
  const tfns = Object.keys(debugFunctionRegistry).filter(
    (fnName) => debugFunctionRegistry[fnName].transitive
  );

  // Build a map of the transitive function calls and a shape list
  const { map: tfnMap, shapes: tfnShapes } = getFnMap(tfns, shapeList);

  // Process the worklist of shapes to propogate transitive propertes.
  const workList: Queue<Shape> = new Queue([...tfnShapes]);
  while (!workList.isEmpty()) {
    const thisShape = workList.dequeue();

    // Repeat this process for each transitive function
    for (const fn in tfns) {
      // Follow the function through each shape it points to.
      // Stop when the chain ends and add each object we find
      // to the fn property of thisShape
      const fnName = tfns[fn];
      const fnPropName = fnName + "[]";
      const shapesOutput: Queue<Shape> = new Queue();
      const shapeWorkList: Queue<Shape> = new Queue([
        ...tfnMap.get(thisShape, fnName),
      ]);

      // Process the worklist of shapes
      while (!shapeWorkList.isEmpty()) {
        const rShape = shapeWorkList.dequeue();

        // Stop a cycle if we detect one
        if (rShape === thisShape) {
          console.error(
            `Stopped cycle in transitive fn ${fnName} on ${getShapeName(
              thisShape
            )} at ${getShapeName(rShape)}`
          );
          continue;
        }

        // Add this shape to our list of output shapes
        shapesOutput.enqueue(rShape);

        // Enqueue any shapes rShape.fn[] points to
        tfnMap.get(rShape, fnName).forEach((e) => {
          shapeWorkList.enqueue(e);
        });
      }

      // Update the function property value
      setStrPropertyValue(
        thisShape,
        fnPropName,
        shapesOutput
          .toArray()
          .map((e) => getShapeName(e))
          .join(";")
      );
    } // for: tfns
  } // while: work exists
};

/**
 * Propogate properties that are upwardly cascadeable.  For example, if
 * A.contains(B) and A.disjoint(C) then B.disjoint(C) is also true.
 *
 * Requires that the shapeList already be populated with shapes and properties,.
 * including transitive function properties.
 *
 * @param shapeList DebugShapeList to add transitives to.
 */
const addCascadesToShapeList = (shapeList: DebugShapeList): void => {
  // Get the list of cascading functions
  const cfns = Object.keys(debugFunctionRegistry).filter(
    (fnName) => debugFunctionRegistry[fnName].cascade
  );

  // Build a map of the cascading function calls and a shape list
  const { map: cfnMap, shapes: cfnShapes } = getFnMap(cfns, shapeList);

  // Process the worklist of shapes to propogate cascading propertes.
  const workList: Queue<Shape> = new Queue([...cfnShapes]);
  while (!workList.isEmpty()) {
    const thisShape = workList.dequeue();
    const thisShapeProps = Object.keys(thisShape.properties);
    const thisShapeFn = cfns.filter((fnName) =>
      thisShapeProps.includes(fnName + "[]")
    );

    // Process each cascading function present on this shape
    for (const fn in thisShapeFn) {
      const fnName = cfns[fn];
      const thisShapeCfn = getFnDef(fnName).cascade?.filter((fnName) =>
        thisShapeProps.includes(fnName + "[]")
      );
      const rShapes = cfnMap.get(thisShape, fnName);

      // Process each fn to cascade upward
      for (const cfn in thisShapeCfn) {
        const cfnName = thisShapeCfn[cfn];
        const cfnDef = getFnDef(cfnName);
        const cfnPropName = cfnName + "[]";

        // Process each target (rShape) of thisShape.fn[]
        for (const i in rShapes) {
          const rShape = rShapes[i];

          // Merge the function properties of thisShape.cfn[] into the shapes
          // the original thisShape.fn[] was pointing to.
          const wasValue = getStrPropertyValue(rShape, cfnPropName, true);
          setStrPropertyValue(
            rShape,
            cfnPropName,
            getStrPropertyValue(thisShape, cfnPropName),
            true
          );
          const nowValue = getStrPropertyValue(rShape, cfnPropName, true);

          // If cfn is a bidi function, then flip the parameters and load
          // the corresponding function property, as well
          if (cfnDef.bidi) {
            // Process only the additional items
            const wasValueSet = new Set(wasValue.split(";"));
            const addValues = nowValue
              .split(";")
              .filter((e) => !wasValueSet.has(e));

            for (const i in addValues) {
              const lShape = getShapeByName(addValues[i], shapeList);

              // Reverse the source and target then load the property value.
              setStrPropertyValue(
                lShape,
                cfnPropName,
                getShapeName(rShape),
                true
              );
            } // for: addValues
          } // if: bidi

          // Put the target shape in the worklist if it changed
          if (wasValue !== nowValue && !workList.toArray().includes(rShape)) {
            workList.enqueue(rShape);
          }
        } // for: rShapes
      } // for: cfns
    } // for: fns
  } // while: work exists
};

/**
 * Returns the DebugFunctionDef for a function.  If a definition is
 * not found, then the default definition is returned.
 *
 * @param fnName Function Name
 * @returns DebugFunctionDef
 */
const getFnDef = (fnName: string): DebugFunctionDef => {
  if (fnName in debugFunctionRegistry) {
    return debugFunctionRegistry[fnName];
  } else {
    return {}; // default: no special defintion
  }
};

/**
 * Builds a map of function calls and a list of shapes matching the set of functions
 * provided as input as fns.
 *
 * @param fns Array of functions to map
 * @param shapeList DebugShapeList, already populated with properties
 * @returns A list of shapes w/matching function calls and a map of those calls.
 */
const getFnMap = (
  fns: string[],
  shapeList: DebugShapeList
): { map: DebugFunctionMap; shapes: Set<Shape> } => {
  const fnShapes: Set<Shape> = new Set(); // Shapes w/matching fns
  const fnMap = new DebugFunctionMap(); // Map of shape fns

  // First build a list of shapes and map of fn calls
  for (const obj in shapeList) {
    // Loop over each object's fields
    for (const field in shapeList[obj]) {
      const thisShape = shapeList[obj][field];

      // Check for the existence of matching functions
      for (const fn in fns) {
        const fnName = fns[fn];
        // If the function exists in the shape's properties
        // add the cfn to the map
        if (fnName + "[]" in thisShape.properties) {
          getStrPropertyValue(thisShape, fnName + "[]")
            .split(";")
            .forEach((e) => {
              fnMap.add(
                shapeList[obj][field],
                fnName,
                resolvePathString(e, shapeList)
              );
            });

          // Add the shape to the list of shapes w/cfns
          fnShapes.add(thisShape);
        }
      } // for: tfns
    } // for: fields
  } // for: objects

  return { map: fnMap, shapes: fnShapes };
};

/**
 * Resolves a path to a shape.  If path not found, throws exception.
 *
 * @param path Path to Shape
 * @param shapeList List of Shapes
 * @param subst? Optional variable substitution
 * @returns Shape that corresponds to path
 */
const resolvePath = (
  path: Path<unknown>,
  shapeList: DebugShapeList,
  subst?: Subst
): Shape => {
  // Check the path length
  if (path.members.length !== 1)
    throw new Error(`Unexpected path length: ${JSON.stringify(path, null, 2)}`);

  // Retrieve the object and field names
  let objName = path.name.contents.value;
  const fieldName = path.members[0].value;

  // Apply subsitutions, if present
  if (subst !== undefined && objName in subst) objName = subst[objName];

  // Return the shape if present in the shapeList
  if (objName in shapeList && fieldName in shapeList[objName])
    return shapeList[objName][fieldName];

  // Throw an exception if we can't find the shape
  throw new Error(`Unable to find path ${objName}.${fieldName} in ShapeList`);
};

/**
 * Resolves a path string to a shape
 *
 * @param path string representing path to Shape
 * @param shapeList List of Shapes
 * @returns Shape that corresponds to path
 */
const resolvePathString = (path: string, shapeList: DebugShapeList): Shape => {
  const [objName, fieldName] = path.split(".");
  if (objName in shapeList && fieldName in shapeList[objName]) {
    return shapeList[objName][fieldName];
  } else {
    throw new Error(`Unable to find path ${objName}.${fieldName} in ShapeList`);
  }
};

/**
 * Gets the name of a shape.
 *
 * @param shape Shape to query for its name
 * @returns string Name of shape
 */
const getShapeName = (shape: Shape): string => {
  if (
    "_shapeName" in shape.properties &&
    shape.properties["_shapeName"].tag === "StrV"
  )
    return shape.properties["_shapeName"].contents;

  throw new Error("Shape missing `_shapeName` string property");
};

/**
 * Finds and returns the shape bearing the requested name.
 *
 * @param shapeName string Name of shape to retrieve
 * @param shapeList DebugShapeList List of Shapes
 * @returns Shape Shape with the matching name.
 */
const getShapeByName = (
  shapeName: string,
  shapeList: DebugShapeList
): Shape => {
  const nameParts = shapeName.split(".");
  if (nameParts.length !== 2)
    throw new Error(`Invalid shape name: ${shapeName}`);
  if (!(nameParts[0] in shapeList && nameParts[1] in shapeList[nameParts[0]]))
    throw new Error(`Unknown shape name: ${shapeName}`);
  return shapeList[nameParts[0]][nameParts[1]];
};

/**
 * Gets the property value from a shape.  Requires that the property value exist
 * and be of type StrV, otherwise an exception is thrown.
 *
 * @param shape Shape to query for its property value
 * @param propName string Property name
 * @returns string Property Value
 */
const getStrPropertyValue = (
  shape: Shape,
  propName: string,
  noException?: boolean
): string => {
  if (!(propName in shape.properties)) {
    if (noException) return "";
    throw new Error(`${getShapeName(shape)}.${propName} does not exist`);
  }
  const thisProp = shape.properties[propName];
  if (thisProp.tag !== "StrV") {
    if (noException) return "";
    throw new Error(`${getShapeName(shape)}.${propName}.tag !== "StrV"`);
  }
  return thisProp.contents;
};

/**
 * Sets the property value of a shape.  If the property does not exist, it will be
 * created.  If the merge option is used, the two values will be merged with duplicates
 * removed.
 *
 * @param shape Shape where the property will be set
 * @param propName string Property name
 * @param propValue string Property value
 * @mparam merge boolean If true, the two values will be merged
 * @returns string Property Value
 */
const setStrPropertyValue = (
  shape: Shape,
  propName: string,
  propValue: string,
  merge?: boolean
): void => {
  let newValue = propValue;
  if (
    merge &&
    propName in shape.properties &&
    shape.properties[propName].tag === "StrV"
  ) {
    // Use the Set to eliminate duplicates
    const newSet = new Set([
      ...getStrPropertyValue(shape, propName).split(";"),
      ...propValue.split(";"),
    ]);
    newValue = [...newSet].join(";");
  }
  shape.properties[propName] = {
    tag: "StrV",
    contents: newValue,
  };
};

/**
 * DebugFunctionMap is a utility class that provides a simple mapping
 * from a function name and a source shape to its target shapes.
 */
class DebugFunctionMap {
  private rep: { [k: string]: Map<Shape, Set<Shape>> } = {};

  /**
   * Maps a source shape and function name to a target shape
   *
   * @param lShape Source shape, for example, A in contains(A,B)
   * @param fn Function, for example "contains"
   * @param rShape Target shape, for example, B in contains(A,B)
   */
  public add(lShape: Shape, fn: string, rShape: Shape) {
    if (!(fn in this.rep)) this.rep[fn] = new Map();
    if (!this.rep[fn].has(lShape)) {
      this.rep[fn].set(lShape, new Set([rShape]));
    } else {
      this.rep[fn].get(lShape)?.add(rShape);
    }
  }

  /**
   * Retrieves any shapes targeted by the lShape and function name.
   *
   * @param lShape Source shape
   * @param fn Function Name
   * @returns A list of shapes targeted by lShape and fn
   */
  public get(lShape: Shape, fn: string): Shape[] {
    if (!(fn in this.rep)) return [];
    const retVal = this.rep[fn].get(lShape);
    if (retVal === undefined) {
      return [];
    } else {
      return [...retVal];
    }
  }
}

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
  const errorList: Set<string> = new Set();

  for (const fn in debugFunctionRegistry) {
    const fnDef = debugFunctionRegistry[fn];

    // Verify the function exists
    if (!(fn in compDict || fn in objDict || fn in constrDict)) {
      errorList.add(
        `Inconsistency in debugFunctionRegistry: Fn not found in compDict, objDict, or constrDict: ${fn}`
      );
    }

    // Verify that no bi-directional function is also cascading up (not supported yet)
    if (fnDef.bidi && fnDef.cascade) {
      errorList.add(
        `Inconsistency in debugFunctionRegistry: Fn cannot be both bidi and cascade: ${fn}`
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
          errorList.add(
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
          errorList.add(
            `Inconsistency in debugFunctionRegistry: Fn '${fn}' contradicts '${contraFnName}', but '${contraFnName}' does not contradict '${fn}'`
          );
        }
      }
    } // if: contra Fn

    // Verify cascade functions exist
    if ("cascade" in fnDef) {
      for (const cascadeFn in fnDef.cascade) {
        const cascadeFnName = fnDef.cascade[cascadeFn];
        if (
          !(
            cascadeFnName in compDict ||
            cascadeFnName in objDict ||
            cascadeFnName in constrDict
          )
        ) {
          errorList.add(
            `Inconsistency in debugFunctionRegistry: cascade Fn not found in compDict, objDict, or constrDict: ${cascadeFnName}`
          );
        }

        if (!(cascadeFnName in debugFunctionRegistry)) {
          errorList.add(
            `Inconsistency in debugFunctionRegistry: cascade Fn not found in debugFunctionRegistry: ${cascadeFnName}`
          );
        }
      } // for: fnDef.cascade
    } // if: cascade Fn
  } // for: debugFunctionRegistry

  return [...errorList];
};

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

  // cascade:
  // --------
  // This is most easily explained by an example: given A.contains(B) and
  // A.disjoint(C), then B.disjoint(C) may be inferred.  This inference is
  // activated for this set of functions as: {contains: {cascade["disjoint"]}}
  cascade?: string[];

  // transitive:
  // ------------
  // Indicates the function is transitive. For example, if A.contains(B) and
  // B.contains(C), then A.contains(C) should also be true.  Configured by:
  // {contains: {transitive: true}}
  transitive?: boolean;

  // bidi: (Bi-directional)
  // ----------------------
  // Bi-directional functions affect every shape in in the function argument list
  // and are applied similarly to each shape in DebugShapeList.  For example, near(A,B)
  // generates properties for both A.near(B) and B.near(A).
  //
  // If bidi is false or not present, the default behavior is used, which applies the
  // function to the first shape argument.
  bidi?: boolean;
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
    cascade: [
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
  onCanvas: {},
  smallerThan: {},
  maxSize: {},
};
