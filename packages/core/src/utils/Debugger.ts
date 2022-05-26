import { A, ASTNode } from "types/ast";
import { DomainProg } from "types/domain";
import { State } from "types/state";
import { StyProg } from "types/style";
import { Subst } from "types/styleSemantics";
import { SubProg } from "types/substance";
import { buildDebugShapeList, DebugShapeList } from "utils/DebugShapeList";

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

  /**
   * Returns the set of GPIs and properties based on Substance object, field,
   * or property name.  If no selection parameters are provided, then all object,
   * fields, and properties are returned.
   *
   * @param inObjName The Substance object name to query
   * @param inFieldName The field name to query
   * @param inPropName The property name to query
   * @returns DebugShapeList of the GPIs and properties, according to the query
   */
  public queryShapeFields(
    inObjName?: string,
    inFieldName?: string,
    inPropName?: string
  ): DebugShapeList {
    // Ensure we are in a suitable state to answer questions
    this.moveToAnsweringState();

    // Happify tsc
    if (this.diagramState === undefined)
      throw new Error(
        "Undefined state - moveToAnsweringState() should prevent this"
      );

    // Build and cache the shape list
    if (this.shapeListCache === undefined) {
      this.shapeListCache = buildDebugShapeList(this.diagramState);
    }

    // ------------------------ Select by Object -----------------------
    // Prune the tree as needed, based on the query parameters
    if (inObjName === undefined) {
      // No object specified - return the entire tree
      return JSON.parse(JSON.stringify(this.shapeListCache)); // Protect the Rep
    } else if (!(inObjName in this.shapeListCache)) {
      // Object was specified but not found
      return {};
    } else if (inFieldName === undefined) {
      // Object specified, but no field - return all fields for object
      return JSON.parse(
        JSON.stringify({ [inObjName]: this.shapeListCache[inObjName] }) // Protect the Rep
      );

      // -------------------- Select by Object, Field --------------------
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

      // --------------- Select by Object, Field, Property ---------------
    } else if (
      !(inPropName in this.shapeListCache[inObjName][inFieldName].properties)
    ) {
      // Object, Field, and Property specified but not found
      return {};
    } else {
      // Object, Field, and Property specified and found
      // Protect the Rep
      return JSON.parse(
        JSON.stringify({
          [inObjName]: {
            [inFieldName]: {
              [inPropName]: this.shapeListCache[inObjName][inFieldName]
                .properties[inPropName],
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
} // class: Debugger

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
