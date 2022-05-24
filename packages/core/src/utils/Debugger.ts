import { constrDict } from "contrib/Constraints";
import { compDict } from "contrib/Functions";
import { objDict } from "contrib/Objectives";
import { A, ASTNode } from "types/ast";
import { DomainProg } from "types/domain";
import { Shape } from "types/shape";
import { State } from "types/state";
import { FieldPath, StyProg } from "types/style";
import { Subst } from "types/styleSemantics";
import { SubProg } from "types/substance";
import { FieldExpr, StrV, TrMap, Value } from "types/value";

/**
 * The Debugger is a singleton that provides a set of queries
 * to analyze the Penrose trio of programs. This class gathers
 * information at compile time to answer these queries as-needed
 * post-compilation.
 *
 * A design goal of this class is to minimize the performance
 * impact on the compilation; hence, little analysis is performed
 * by the debugger until the first query is executed.
 *
 * Note: If compiling multiple trios, it is necessary to call
 * Debugger.newInstance() before compiling each trio.  This may
 * be seen in penrose/core/compileTrio().
 */
export class Debugger {
  private state = 0; // 0=Listening, 1=Answering
  private static theInstance?: Debugger; // The singleton instance is created lazily
  private rep: DebugStyleBlock[] = []; // Style blocks
  private domSrc = ""; // Source code of the Domain program
  private subSrc = ""; // Source code of the Substance program
  private stySrc = ""; // Source code of the Style program
  private domAst?: DomainProg<A>; // AST of the Domain program
  private subAst?: SubProg<A>; // AST of the Substance program
  private styAst?: StyProg<A>; // AST of the Style program
  private diagramState?: State; // Current diagram state
  private shapeListCache?: DebugShapeList; // Cache of Shape List

  // !!!
  private static debugFunctions: {
    [k: string]: DebugFunctionDef;
  } = {
    // ---------------------------- Objective Functions ---------------------//

    minimal: {},
    maximal: {},
    equal: {},
    greaterThan: {},
    lessThan: {},
    repelPt: {
      contradicts: ["nearPt"],
    },
    repelScalar: {},
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
      contradicts: [
        "near",
        "sameCenter",
        "contains",
        "overlapping",
        "touching",
      ],
      bidi: true,
    },
    near: {
      contradicts: ["repel"],
      bidi: true,
    },
    nearPt: {
      contradicts: ["repelPt"],
    },
    nonDegenerateAngle: {},
    centerArrow: {},
    centerLabelAbove: {},
    centerLabel: {},
    pointLineDist: {},

    // --------------------------- Constraint Functions ---------------------//

    //equal: {},
    //lessThan: {},
    //greaterThan: {},
    touching: {
      contradicts: ["repel", "disjoint"],
      bidi: true,
    },
    lessThanSq: {},
    greaterThanSq: {},
    inRange: {},
    contains1D: {},
    disjointScalar: {},
    perpendicular: {},
    collinear: {},
    collinearUnordered: {},
    onCanvas: {},
    minSize: {},
    maxSize: {},
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
    },
    atDist: {},
    smallerThan: {},
    ptCircleIntersect: {},
    disjointIntervals: {},

    // --------------------------- Computation Functions --------------------//

    average: {},
    makePath: {},
    get: {},
    rgba: {},
    selectColor: {},
    hsva: {},
    none: {},
    acosh: {},
    acos: {},
    asin: {},
    asinh: {},
    atan: {},
    atan2: {},
    atanh: {},
    cbrt: {},
    ceil: {},
    cos: {},
    cosh: {},
    exp: {},
    expm1: {},
    floor: {},
    log: {},
    log2: {},
    log10: {},
    log1p: {},
    pow: {},
    round: {},
    sign: {},
    sin: {},
    sinh: {},
    tan: {},
    tanh: {},
    trunc: {},
    dot: {},
    lineLength: {},
    len: {},
    concat: {},
    normalize: {},
    pathFromPoints: {},
    quadraticCurveFromPoints: {},
    cubicCurveFromPoints: {},
    unitMark: {},
    unitMark2: {},
    arc: {},
    wedge: {},
    ptOnLine: {},
    arcSweepFlag: {},
    angleBetween: {},
    angleFrom: {},
    cross2D: {},
    lineLineIntersection: {},
    midpoint: {},
    midpointOffset: {},
    chevron: {},
    innerPointOffset: {},
    ticksOnLine: {},
    orientedSquare: {},
    intersectingSideSize: {},
    triangle: {},
    average2: {},
    unit: {},
    sampleColor: {},
    setOpacity: {},
    mul: {},
    barycenter: {},
    circumcenter: {},
    circumradius: {},
    incenter: {},
    inradius: {},
    sqr: {},
    sqrt: {},
    max: {},
    min: {},
    abs: {},
    toRadians: {},
    toDegrees: {},
    norm: {},
    normsq: {},
    vdist: {},
    vmul: {},
    vdistsq: {},
    angleOf: {},
    MathE: {},
    MathPI: {},
    rot90: {},
    rotateBy: {},
  };

  // ------------------------- Singleton Impl. -----------------------------//

  /**
   * Replaces the singleton debugger with a new instance, i.e., for when
   * a new program is being compiled/debugged.
   *
   * @returns A new instance of the debugger
   */
  public static newInstance(): Debugger {
    this.theInstance = new Debugger();
    return this.theInstance;
  }
  /**
   * Returns the current instance of the debugger.
   *
   * @returns The current instance of the debugger
   */
  public static getInstance(): Debugger {
    if (Debugger.theInstance === undefined) {
      return Debugger.newInstance();
    } else {
      return Debugger.theInstance;
    }
  }

  // ----------------------- Accessors / Mutators --------------------------//

  /**
   * Adds a style block to the Debugger, including sat/unsat reasons
   *
   * @param block The Style block to add
   */
  public addBlock(block: DebugStyleBlock): void {
    this.moveToListeningState();
    this.rep.push(block); // We do not clone the block here for performance reasons
  }
  /**
   * Returns a copy of the Style blocks currently in the debugger
   *
   * @returns A copy of the Style blocks currently in the debugger
   */
  public getBlocks(): DebugStyleBlock[] {
    return JSON.parse(JSON.stringify(this.rep));
  }
  /**
   * Loads the source code of the Domain program into the Debugger
   *
   * @param domSrc The source code of the Domain program
   */
  public setDomSrc(domSrc: string): void {
    this.moveToListeningState();
    this.domSrc = domSrc;
  }
  /**
   * Loads the source code of the Substance program into the Debugger
   *
   * @param subSrc The source code of the Substance program
   */
  public setSubSrc(subSrc: string): void {
    this.moveToListeningState();
    this.subSrc = subSrc;
  }
  /**
   * Loads the source code of the Style program into the Debugger
   *
   * @param stySrc The source code of the Style program
   */
  public setStySrc(stySrc: string): void {
    this.moveToListeningState();
    this.stySrc = stySrc;
  }
  /**
   * Loads the AST of the Domain program into the Debugger
   *
   * @param domAst The AST of the Domain program
   */
  public setDomAst(domAst: DomainProg<A>): void {
    this.moveToListeningState();
    this.domAst = domAst; // We do not clone the AST here for performance reasons
  }
  /**
   * Loads the AST of the Substance program into the Debugger
   *
   * @param subAst The AST of the Substance program
   */
  public setSubAst(subAst: SubProg<A>): void {
    this.moveToListeningState();
    this.subAst = subAst; // We do not clone the AST here for performance reasons
  }
  /**
   * Loads the AST of the Style program into the Debugger
   *
   * @param styAst The AST of the Style program
   */
  public setStyAst(styAst: StyProg<A>): void {
    this.moveToListeningState();
    this.styAst = styAst; // We do not clone the AST here for performance reasons
  }

  // !!!
  public setState(diagramState: State): void {
    this.moveToListeningState();
    this.diagramState = diagramState;
  }

  // ----------------------------- Queries ---------------------------------//

  /**
   * This query returns true if the style block at styLine with style variable
   * and object mapping relVars satisfied the style block's requirements.
   *
   * Exception thrown if no style block is present at styLine.
   *
   * @param styLine The line number of the style block
   * @param relVars Mapping of Style variables to Substance objects for the style block
   * @returns True if the style block applied to the substance object; false, otherwise
   */
  public queryDoesStyleBlockApply(styLine: number, relVars: Subst): boolean {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    // Get the style block for this line number
    const theBlock = this.getBlockAtLineNumber(styLine);

    // If we did not find the style block specified, throw an exception
    if (theBlock === undefined) {
      throw new Error(`Style block at line ${styLine} not found`);
    } else {
      // Check the list of successful substitutions captured during Style compilation.
      let allVarsMatch;
      for (const i in theBlock.substs) {
        const blockVars = theBlock.substs[i];
        allVarsMatch = true;
        for (const j in relVars) {
          if (j in blockVars && blockVars[j] === relVars[j]) {
            // Do nothing
          } else {
            allVarsMatch = false;
            break;
          }
        }
        if (allVarsMatch) {
          return true;
        }
      }
      return false; // Default case = no match
    }
  }

  /**
   * Returns true if the style block at styLine has a where clause
   *
   * Exception thrown if no style block is present at styLine.
   *
   * @param styLine The line number of the style block
   * @returns True if the style block has a where clause; false, otherwise
   */
  public queryDoesStyleBlockHaveWhereClause(styLine: number): boolean {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    // Get the style block for this line number
    const theBlock = this.getBlockAtLineNumber(styLine);

    // If we did not find the style block specified, throw an exception
    if (theBlock === undefined) {
      throw new Error(`Style block at line ${styLine} not found`);
    } else {
      return theBlock.hasWhereClause;
    }
  }

  /**
   * Returns the set of reasons why a style block where clause was or was not satisfied.
   * If the block lacks a where clause, an empty set of reasons is returned.
   *
   * Exception thrown if no style block is present at styLine.
   *
   * @param styLine The line number of the style block
   * @param relVars Mapping of Style variables to Substance objects for the style block
   * @returns A list of reasons why the where clause was satisfied or not satisfied
   */
  public queryExplainStyleBlockApplication(
    styLine: number,
    relVars: Subst
  ): DebugStyleBlockRel[] {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    // Get the style block for this line number
    const theBlock = this.getBlockAtLineNumber(styLine);

    // If we did not find the style block specified, throw an exception
    if (theBlock === undefined) {
      throw new Error(`Style block at line ${styLine} not found`);
    } else {
      // If there is no where clause, return an empty array
      if (!theBlock.hasWhereClause) return [];

      // Return the matching sats/unsat conditions.  Protect the rep.
      const conds = this.queryDoesStyleBlockApply(styLine, relVars)
        ? theBlock.sats
        : theBlock.unsats;
      return JSON.parse(
        JSON.stringify(
          conds.filter((rel) =>
            this.hasMatchingSubstitution(rel, theBlock, relVars)
          )
        )
      );
    }
  }

  // !!!
  public queryShapeFields(
    inObjName?: string,
    inFieldName?: string,
    inPropName?: string
  ): DebugShapeList {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    // Build and cache the shape list
    if (this.shapeListCache === undefined) {
      this.shapeListCache = this.buildShapeList();
    }

    // Prune the tree as needed, based on the query parameters
    if (inObjName === undefined) {
      // No object specified - return all objects
      return JSON.parse(JSON.stringify(this.shapeListCache)); // Protect the Rep
    } else if (!(inObjName in this.shapeListCache)) {
      // Object specified but not found
      return {};
    } else if (inFieldName === undefined) {
      // Object specified, but no field - return all fields
      return JSON.parse(
        JSON.stringify({ [inObjName]: this.shapeListCache[inObjName] }) // Protect the Rep
      );
    } else if (!(inFieldName in this.shapeListCache[inObjName])) {
      // Object and field specified but not found
      return {};
    } else if (inPropName === undefined) {
      // Object and field specified, but no property -- return all properties
      return JSON.parse(
        JSON.stringify({
          [inObjName]: {
            [inFieldName]: this.shapeListCache[inObjName][inFieldName], // Protect the Rep
          },
        })
      );
    } else if (!(inPropName in this.shapeListCache[inObjName][inFieldName])) {
      // Object, Field, and Property specified but not found
      return {};
    } else {
      // Object, Field, and Property specified and found
      // Protect the Rep
      return JSON.parse(
        JSON.stringify({
          [inObjName]: {
            [inFieldName]: {
              [inPropName]: this.shapeListCache[inObjName][inFieldName][
                inPropName
              ],
            },
          },
        })
      );
    }
  }

  // -------------------------- State Control ------------------------------//

  /**
   * Moves the Debugger to the Listening state.
   */
  private moveToListeningState(): void {
    this.shapeListCache = undefined;
    this.state = 0;
  }

  /**
   * Moves the Debugger to the Answering state.  Raises an exception if the
   * ASTs and source core are not loaded.
   */
  private moveToAnsweringState(): void {
    if (this.state >= 1) {
      // Do nothing: no state change required
    } else {
      if (this.domAst === undefined)
        throw new Error("Unable to accept debug queries: no Domain AST loaded");
      if (this.subAst === undefined)
        throw new Error(
          "Unable to accept debug queries: no Substance AST loaded"
        );
      if (this.styAst === undefined)
        throw new Error("Unable to accept debug queries: no Style AST loaded");
      if (this.domSrc === "")
        throw new Error(
          "Unable to accept debug queries: no Domain Source File loaded"
        );
      if (this.subSrc === "")
        throw new Error(
          "Unable to accept debug queries: no Substance Source File loaded"
        );
      if (this.stySrc === "")
        throw new Error(
          "Unable to accept debug queries: no Style Source File loaded"
        );
      if (this.diagramState === undefined)
        throw new Error(
          "Unable to accept debug queries: no diagram state loaded"
        );

      // Transition to answering state:
      this.state = 1; // Answering

      // Resolve source text references (alt. approach: do this lazily)
      this.addSourceToRefs();
    }
  }

  /**
   * Indicates the Debugger is currently listening to the system for input;
   * that is, it has not yet received a user API query.
   *
   * @returns true if the Debugger is listening for system input; false otherwise
   */
  public isListening(): boolean {
    return !this.state;
  }

  /**
   * Indicates the Debugger is currently answering user API calls;
   * that is, it is not currently listening for input from the system.
   *
   * @returns true if the Debugger is answering API queries; false otherwise
   */
  public isAnswering(): boolean {
    return Boolean(this.state);
  }

  // ------------------------- Helper Functions ----------------------------//

  /**
   * Finds source code references without source text and adds source text.
   */
  private addSourceToRefs(): void {
    this.moveToAnsweringState();
    this.rep.forEach((block) => {
      // Resolve Block Ref
      if (block.blockRef.srcText === undefined) {
        block.blockRef.srcText = Debugger.getSourceText(
          this.stySrc,
          block.blockRef
        );
      }

      // Loop over each relation in the block
      block.unsats.concat(block.sats).forEach((sat) => {
        // Resolve source for the relation
        if (sat.relRef.srcText === undefined) {
          sat.relRef.srcText = Debugger.getSourceText(this.stySrc, sat.relRef);
        }

        // Resolve source for each reason
        sat.reasons.forEach((reason) => {
          reason.srcRef.forEach((srcRef) => {
            if (srcRef.srcText === undefined) {
              srcRef.srcText = Debugger.getSourceText(this.subSrc, srcRef);
            }
          });
        });
      });
    });
  }

  /**
   * Internal helper function to check if a given block substitution applies to the query.
   *
   * @param rel The relation we are checking to see if it matches the query
   * @param block The block where the relation is defined that matches the query
   * @param relVars The mapping of style and substance variables we are looking for in the relation
   * @returns true if the relation matches the query, otherwise false
   */
  private hasMatchingSubstitution = (
    rel: DebugStyleBlockRel,
    block: DebugStyleBlock,
    relVars: Subst
  ): boolean => {
    // Throw an exception if provided an empty query
    if (Object.keys(relVars).length === 0) {
      throw new Error(`Debug query has no Style variable substitution`);
    }

    // Loop over the substitution pairs in the query
    for (const k in relVars) {
      // If the queried style variable exists in the block...
      if (k in rel.subst) {
        // ... and the bound Substance variables do not match, return false
        if (relVars[k] !== rel.subst[k]) {
          return false;
        }
      } else {
        // ... otherwise throw an exception if the style variable does not exist
        throw new Error(
          `Style variable '${relVars[k]}' not found in Style block at lines ${block.blockRef["start"].line}-${block.blockRef["end"].line}`
        );
      }
    }

    // If we found no mismatches, return true
    return true;
  };

  /**
   * Finds the style block that matches the given line number
   *
   * @param line The line number of the style block
   * @returns The style block at that line number, otherwise undefined
   */
  private getBlockAtLineNumber(styLine: number): DebugStyleBlock | undefined {
    // Loop over each style block to find the one specified on input
    for (const i in this.rep) {
      const block = this.rep[i];
      // Find the block specified by the line number; ignore the rest
      if (
        block.blockRef.lineStart <= styLine &&
        block.blockRef.lineEnd >= styLine
      ) {
        return block;
      }
    }
    return undefined;
  }

  /**
   * Returns source text for the given start and end positions
   *
   * @param pgmSrc Program source
   * @param srcRef Reference to lines and columns to return from source
   * @returns Source text
   */
  public static getSourceText(
    pgmSrc: string,
    srcRef: DebugSourceRef
  ): string[] {
    // Locally adjust the lines and columns from the AST for processing here
    const startLine: number = srcRef.lineStart - 1;
    const startCol: number = srcRef.colStart;
    const endLine: number = srcRef.lineEnd - 1;
    const endCol: number = srcRef.colEnd + 1;

    // Split the source into lines
    const lines = pgmSrc.split(/\r?\n/);
    const outLines: string[] = [];

    // Check that the inputs are in range
    if (srcRef.lineStart < 1 || srcRef.lineStart > srcRef.lineEnd) {
      throw new Error(
        "Invalid Source Ref: startLine must be > 0 and < endLine"
      );
    }
    if (srcRef.lineEnd > lines.length) {
      throw new Error(
        `Invalid Source Ref: endLine must be <= last source line)`
      );
    }
    if (srcRef.colStart < 0) {
      throw new Error("Invalid Source Ref: colStart must be >= 0");
    }

    // Loop over the lines in the source to find the source Text
    for (let i = startLine; i < endLine + 1; i++) {
      if (i >= startLine && i <= endLine) {
        if (startLine === endLine) {
          if (startCol <= endCol - 1) {
            outLines.push(lines[i].substring(startCol, endCol));
          } else {
            outLines.push(lines[i].substring(startCol));
          }
        } else if (i === startLine) {
          outLines.push(lines[i].substring(startCol));
        } else if (i === endLine) {
          outLines.push(lines[i].substring(0, endCol));
        } else {
          outLines.push(lines[i]);
        }
      }
    }
    return outLines;
  }

  /**
   * Returns a DebugSourceRef that corresponds to the AST Source Node.
   * Note: This routine does not resolve the source text.
   *
   * @param node AST Source Node
   * @returns DebugSourceRef
   */
  public static getSourceRefFromAstNode(node: ASTNode<A>): DebugSourceRef {
    // First check that the node has a set of source references
    if (
      "start" in node &&
      "line" in node["start"] &&
      "col" in node["start"] &&
      "end" in node &&
      "line" in node["end"] &&
      "col" in node["end"]
    ) {
      // Do nothing
    } else {
      throw new Error(
        `Invalid AST Node lacks start/end source refs: ${JSON.stringify(node)}`
      );
    }

    // Determine the origin program of the node based on its type
    let origin: DebugProgramType;
    switch (node.nodeType) {
      case "Domain": {
        origin = DebugProgramType.DOMAIN;
        break;
      }
      case "SyntheticStyle":
      case "Style": {
        origin = DebugProgramType.STYLE;
        break;
      }
      case "SyntheticSubstance":
      case "Substance": {
        origin = DebugProgramType.SUBSTANCE;
        break;
      }
      default:
        throw new Error(`Unexpected AST node type: ${node.nodeType}`);
    }

    // Return the source reference
    return {
      origin: origin,
      lineStart: node["start"].line,
      lineEnd: node["end"].line,
      colStart: node["start"].col,
      colEnd: node["end"].col,
    };
  }

  /**
   * !!! Purpose of this routine is to build a data structure useful for debugging
   * that relates the underlying Substance objects to the individual shapes and
   * values.
   *
   * TODO Break up this routine -- it's too long !!!
   *
   * @returns DebugShapeList
   */
  private buildShapeList(): DebugShapeList {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    if (this.diagramState === undefined) return {}; // happify tsc

    const translation: TrMap<A> = this.diagramState.translation.trMap; // Ref. to translation map in diagram state
    const shapes: Shape[] = this.diagramState.shapes; // Ref. to Shapes in diagram state
    const shapeList: DebugShapeList = {}; // List of Shapes to output
    const compList: FieldExpr<A>[] = []; // List of computations to process
    const cascadeList: {
      [k: number]: {
        comp: FieldExpr<A>;
        done: boolean;
        incoming: Shape;
        outgoing: Shape[];
        fnName: string;
        fnDef: DebugFunctionDef;
      };
    } = {}; // List of computations to cascade (post-process)
    let cascadeCounter = 0;

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

        // Copy the shape from the Shape State.  We copy from shapes instead of
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
          thisComp.contents.contents.name.value in Debugger.debugFunctions
        )
      ) {
        console.log(`Skipping comp: ${JSON.stringify(thisComp)}`); // !!!
        continue;
      }

      const fnName = thisComp.contents.contents.name.value; // Function name
      const fnDef = Debugger.debugFunctions[fnName]; // Function definition in debugger
      const fnPropName = fnName + "[]"; // Name of the function property
      const fnShapeArgs: Shape[] = []; // Shape arguments to the computation

      // Accumulate all the shape arguments
      for (const i in thisComp.contents.contents.args) {
        const thisArg = thisComp.contents.contents.args[i];
        if (thisArg.tag === "FieldPath") {
          fnShapeArgs.push(Debugger.resolveFieldPath(thisArg, shapeList));
        }
      } // for: function shape arguments

      console.log(`${fnName} has ${fnShapeArgs.length} shape args`); // !!!

      // If there are no arguments, skip the computation
      if (!fnShapeArgs.length) continue;

      // Bidirectional Mapping
      // ---------------------
      // Create properties for each shape argument to represent the constraint
      // Functions classified as 'bi-directional' apply qually to all shapes
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
            shapeProperties[fnPropName] = Debugger.addToShapePropertyList(
              argsMinusThisShape,
              fnProp
            );
          } else {
            shapeProperties[fnPropName] = Debugger.addToShapePropertyList(
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
        } // fnShapeArgs
      } else {
        // Unidirectional Mapping (default)
        // --------------------------------
        // Creates one property on the shape to represent the computation and its other
        // shape arguments.  The base shape is the first argument.
        if (fnShapeArgs.length) {
          const shape = fnShapeArgs.shift(); // Take the first shape from the argument list
          if (shape === undefined) continue; // Happify tsc

          // TODO !!! This doesn't work because it lacks a check for uniqueness

          const fnProp: Value<number> | undefined =
            fnPropName in shape.properties
              ? shape.properties[fnPropName]
              : undefined;

          // Add the shapes to the property
          if (fnProp && fnProp.tag === "StrV") {
            shape.properties[fnPropName] = Debugger.addToShapePropertyList(
              fnShapeArgs,
              fnProp
            );
          } else {
            shape.properties[fnPropName] = Debugger.addToShapePropertyList(
              fnShapeArgs
            );
          }
          console.log(
            `Set UniDi ${JSON.stringify(
              shape.properties.name.contents
            )}.${fnPropName} = ${JSON.stringify(
              shape.properties[fnPropName].contents
            )}`
          ); // !!!

          // Defer cascades until after we process all the uni-directional properties.
          if (fnDef.cascade) {
            cascadeList[cascadeCounter++] = {
              comp: thisComp,
              done: false,
              incoming: shape,
              outgoing: fnShapeArgs,
              fnName: fnName,
              fnDef: fnDef,
            };
          }
        }
      } // else: uni-directional
    } // while: computations to process

    // Loop over the list of cascades we deferred previously.  Due to cascading, multiple
    // iterations might be needed to stabalize all properties. !!!
    let workRemains = true;
    while (workRemains) {
      console.log("---------Cascade Pass Begins---------"); // !!!
      workRemains = false;

      for (const i in cascadeList) {
        console.log(`cascadeList[${i}]`); // !!!
        const thisCascade = cascadeList[i];
        let changesMade = false;

        if (thisCascade.done) continue; // Skip complete work items

        for (const cascadeFn in thisCascade.fnDef.cascade) {
          const fnPropName = thisCascade.fnDef.cascade[cascadeFn] + "[]";
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
            console.log(`    - shape [${j}`); // !!!
            const outShape = thisCascade.outgoing[j];
            const inProperty = fnProperties[fnPropName];
            let oldPropString = "";
            let newPropString = "";
            const newPropValue =
              inProperty.tag === "StrV"
                ? new Set(inProperty.contents.split(";"))
                : new Set();
            console.log(`0: ${JSON.stringify([...newPropValue])}`); // !!!

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
              console.log(`1: ${JSON.stringify([...newPropValue])}`); // !!!
              newPropString = [...newPropValue].join(";");
              console.log(`2: ${JSON.stringify([...newPropValue])}`); // !!!
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
              console.log(`    Was: ${oldPropString}`); // !!!
              console.log(`    Now: ${newPropString}`); // !!!
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
  }

  /**
   * !!!
   *
   * @param shapesToAdd
   * @param shapeProp
   * @returns
   */
  private static addToShapePropertyList(
    shapesToAdd: Shape[],
    shapeProp?: StrV
  ): StrV {
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
  }

  /**
   * !!!
   *
   * @param fieldPath
   * @param shapeList
   * @returns
   */
  private static resolveFieldPath(
    fieldPath: FieldPath<unknown>,
    shapeList: DebugShapeList
  ): Shape {
    const objName = fieldPath.name.contents.value;
    const fieldName = fieldPath.field.value;
    if (objName in shapeList && fieldName in shapeList[objName]) {
      return shapeList[objName][fieldName];
    } else {
      throw new Error(
        `Unable to field fieldPath ${objName}.${fieldName} in ShapeList`
      );
    }
  }

  /**
   * !!! Look for invalid function defintions
   *
   * @returns
   */
  public static checkFunctionRegistryConsistenvy(): string[] {
    const errorList: string[] = [];

    for (const fn in Debugger.debugFunctions) {
      const fnDef = Debugger.debugFunctions[fn];

      // Verify the function exists
      if (!(fn in compDict || fn in objDict || fn in constrDict)) {
        errorList.push(
          `Inconsistency in Debugger.debugFunctions: Fn not found: ${fn}`
        );
      }

      // Verify that no bi-directional function is also cascading (not supported yet)
      if (fnDef.bidi && fnDef.cascade) {
        errorList.push(
          `Inconsistency in Debugger.debugFunctions: Fn cannot be both bidi and cascading: ${fn}`
        );
      }

      // Verify contradiction functions exist
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
              `Inconsistency in Debugger.debugFunctions: contraFn not found: ${contraFnName}`
            );
          }

          // Check that any contradiction is bi-directional
          const contraFnDef = Debugger.debugFunctions[contraFnName];
          if (
            !(
              contraFnName in Debugger.debugFunctions &&
              contraFnDef.contradicts !== undefined &&
              contraFnDef.contradicts.includes(fn)
            )
          ) {
            errorList.push(
              `Inconsistency in Debugger.debugFunctions: Fn '${fn}' contradicts '${contraFnName}', but '${contraFnName}' does not contradict '${fn}'`
            );
          }
        }
      } // contraFn

      // Verify cascading functions exist
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
            errorList.push(
              `Inconsistency in Debugger.debugFunctions: cascadeFn not found: ${cascadeFnName}`
            );
          }
        }
      } // cascadeFn

      // Check that all functions in the dictionaries are in the debug registry
      for (const compFn in compDict) {
        if (!(compFn in Debugger.debugFunctions)) {
          errorList.push(
            `Inconsistency: '${compFn} found in compDict but not in Debugger.debugFunctions`
          );
        }
      } // compDict
      for (const objFn in compDict) {
        if (!(objFn in Debugger.debugFunctions)) {
          errorList.push(
            `Inconsistency: '${objFn} found in objDict but not in Debugger.debugFunctions`
          );
        }
      } // objDict
      for (const constrFn in constrDict) {
        if (!(constrFn in Debugger.debugFunctions)) {
          errorList.push(
            `Inconsistency: '${constrFn} found in constrDict but not in Debugger.debugFunctions`
          );
        }
      } // objDict
    }
    return errorList;
  }
} // End of Debugger class

// ------------------------------- Types ---------------------------------//

// Represents a single Style block
export type DebugStyleBlock = {
  blockRef: DebugSourceRef; // Selection Block Reference
  substs: Subst[]; // List of substitutions tried
  hasWhereClause: boolean; // true = Where clause present, false otherwise
  sats: DebugStyleBlockRel[]; // List of satisfied relations (if not a match-all block)
  unsats: DebugStyleBlockRel[]; // List of unsatisfied relations (if not a match-all block)
};

// Represents a single Style block relation from a where clause
export type DebugStyleBlockRel = {
  subst: Subst; // Variable substitution mapping (style variable -> substance variable)
  relRef: DebugSourceRef; // Source reference to a where clause relation
  reasons: DebugReason[]; // Reasons the relation is satisfied or unsatisfied
};

// Explanation of why a relation is satisfied or unsatisfied
export type DebugReason = {
  code: DebugReasonCodes; // Reason code (see below)
  srcRef: DebugSourceRef[]; // Source code references providing context for the reason
};

// A reference to a specific section of a source program
export type DebugSourceRef = {
  origin: DebugProgramType; // e.g. DOMAIN, SUBSTANCE, STYLE
  lineStart: number; // Start line number of the source reference (minimum=1)
  lineEnd: number; // Ending line number of the source reference
  colStart: number; // Start column number of the source reference (minimum=0)
  colEnd: number; // Ending column number of the source reference
  srcText?: string[]; // Source text of the entity
};

// One of the three types of programs
export enum DebugProgramType {
  DOMAIN = "Domain",
  SUBSTANCE = "Substance",
  STYLE = "Style",
}

// Reasons codes used by the Debugger to provide explanations in queries
export enum DebugReasonCodes {
  MATCHING_SUB_STATEMENTS_FOUND = "MATCHING_SUB_STATEMENTS_FOUND",
  NO_MATCHING_SUB_STATEMENTS_FOUND = "NO_MATCHING_SUB_STATEMENTS_FOUND",
}

// !!!
export type DebugShapeList = Record<string, Record<string, Shape>>;

// !!!
type DebugFunctionDef = {
  contradicts?: string[];
  cascade?: string[];
  bidi?: boolean;
};
