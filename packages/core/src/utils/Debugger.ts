import { A, ASTNode } from "types/ast";
import { DomainProg } from "types/domain";
import { StyProg } from "types/style";
import { Subst } from "types/styleSemantics";
import { SubProg } from "types/substance";

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
  private state = 0; // 0=listening, 1=answering
  private static theInstance: Debugger;
  private rep: DebugStyleBlock[] = [];
  private domSrc = "";
  private subSrc = "";
  private stySrc = "";
  private domAst?: DomainProg<A>;
  private subAst?: SubProg<A>;
  private styAst?: StyProg<A>;

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
    if (this.theInstance == undefined) {
      return Debugger.newInstance();
    } else {
      return this.theInstance;
    }
  }

  // ------------------------ Setters / Getters ----------------------------//

  public addBlock(block: DebugStyleBlock): void {
    this.moveToListeningState();
    this.rep.push(block);
  }
  public getBlocks(): DebugStyleBlock[] {
    return JSON.parse(JSON.stringify(this.rep));
  }
  public setDomSrc(domSrc: string): void {
    this.moveToListeningState();
    this.domSrc = domSrc;
  }
  public setSubSrc(subSrc: string): void {
    this.moveToListeningState();
    this.subSrc = subSrc;
  }
  public setStySrc(stySrc: string): void {
    this.moveToListeningState();
    this.stySrc = stySrc;
  }
  public setDomAst(domAst: DomainProg<A>): void {
    this.moveToListeningState();
    this.domAst = domAst;
  }
  public setSubAst(subAst: SubProg<A>): void {
    this.moveToListeningState();
    this.subAst = subAst;
  }
  public setStyAst(styAst: StyProg<A>): void {
    this.moveToListeningState();
    this.styAst = styAst;
  }

  // ----------------------------- Queries ---------------------------------//

  /**
   * This query returns true if the style block at styLine with style variable
   * styVar bound to substance object subObj satisfied the style block's requirements.
   *
   * Exception thrown if no style block is present at styLine.
   *
   * @param styLine The line number of the style block
   * @param relVars Mapping of Style variables to Substance objects for the style block
   * @returns True if the style block applied to the substance object, false otherwise
   */
  public queryDidStyleBlockApply(styLine: number, relVars: Subst): boolean {
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
          if (j in blockVars && blockVars[j] == relVars[j]) {
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
   * @returns True if the style block has a where clause, false otherwise
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
      const conds = this.queryDidStyleBlockApply(styLine, relVars)
        ? theBlock.sats
        : theBlock.unsats;
      return JSON.parse(
        JSON.stringify(
          conds.filter((rel) =>
            this.hasMatchingSubstitution(
              rel,
              theBlock as DebugStyleBlock,
              relVars
            )
          )
        )
      );
    }
  }

  // -------------------------- State Control ------------------------------//

  private moveToListeningState(): void {
    this.state = 0;
  }
  private moveToAnsweringState(): void {
    if (this.state >= 1) {
      return; // No state change required
    } else {
      if (this.domAst === undefined)
        throw new Error("Unable to accept debug queries: no Domain AST loaded");
      if (this.subAst === undefined)
        throw new Error(
          "Unable to accept debug queries: no Substance AST loaded"
        );
      if (this.styAst === undefined)
        throw new Error("Unable to accept debug queries: no Style AST loaded");
      if (this.domSrc == "")
        throw new Error(
          "Unable to accept debug queries: no Domain Source File loaded"
        );
      if (this.subSrc == "")
        throw new Error(
          "Unable to accept debug queries: no Substance Source File loaded"
        );
      if (this.stySrc == "")
        throw new Error(
          "Unable to accept debug queries: no Style Source File loaded"
        );

      // Transition to listening state:
      //  - Set the state to answering state
      //  - Generate the source map
      //  - Add source text to reasons
      // (Might want to consider lazy-evaluating this)
      this.state = 1; // Answering
      this.addSourceToRefs();
    }
  }

  // ------------------------- Helper Functions ----------------------------//

  private addSourceToRefs() {
    this.moveToAnsweringState();
    this.rep.forEach((block) => {
      // Resolve Block Ref
      block.blockRef.srcText = this.getSourceText(this.stySrc, block.blockRef);

      // Loop over each relation in the block
      block.unsats.concat(block.sats).forEach((sat) => {
        // Resolve source for the relation
        sat.relRef.srcText = this.getSourceText(this.stySrc, sat.relRef);

        // Resolve source for each reason
        sat.reasons.forEach((reason) => {
          reason.srcRef.forEach((srcRef) => {
            srcRef.srcText = this.getSourceText(this.stySrc, srcRef);
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
    if (relVars == {}) {
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
        block.blockRef["start"].line <= styLine &&
        block.blockRef["end"].line >= styLine
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
  private getSourceText(pgmSrc: string, srcRef: DebugSourceRef): string[] {
    // Locally adjust the lines and columns from the AST for processing here
    const startLine: number = srcRef.lineStart - 1;
    const startCol: number = srcRef.colStart;
    const endLine: number = srcRef.lineEnd - 1;
    const endCol: number = srcRef.colEnd + 1;

    // Split the source into lines
    const lines = pgmSrc.split(/\r?\n/);
    const outLines: string[] = [];

    // Loop over the lines in the source to find the source Text
    for (let i = startLine; i < endLine + 1; i++) {
      if (i >= startLine && i <= endLine) {
        if (startLine == endLine) {
          if (startCol <= endCol) {
            outLines.push(lines[i].substring(startCol, endCol));
          } else {
            outLines.push(lines[i].substring(startCol));
          }
        } else if (i == startLine) {
          outLines.push(lines[i].substring(startCol));
        } else if (i == endLine) {
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
      throw new Error(`Invalid AST Node lacks start/end source refs: ${node}`);
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
}

export type DebugStyleBlock = {
  blockRef: DebugSourceRef; // Selection Block Reference
  substs: Subst[]; // List of substitutions tried
  hasWhereClause: boolean; // true = Where clause present, false otherwise
  sats: DebugStyleBlockRel[]; // List of satisfied relations (if not a match-all block)
  unsats: DebugStyleBlockRel[]; // List of unsatisfied relations (if not a match-all block)
};
export type DebugStyleBlockRel = {
  subst: Subst;
  relRef: DebugSourceRef;
  reasons: DebugReason[];
};
export type DebugReason = {
  code: DebugReasonCodes;
  srcRef: DebugSourceRef[];
  srcTxt?: string[];
};
export type DebugSourceRef = {
  origin: DebugProgramType; // e.g. DOMAIN, SUBSTANCE, STYLE
  lineStart: number;
  lineEnd: number;
  colStart: number;
  colEnd: number;
  srcText?: string[]; // Source text of the entity
};
export enum DebugProgramType {
  DOMAIN = "Domain",
  SUBSTANCE = "Substance",
  STYLE = "Style",
}
export enum DebugReasonCodes {
  MATCHING_SUB_STATEMENTS_FOUND = "MATCHING_SUB_STATEMENTS_FOUND",
  NO_MATCHING_SUB_STATEMENTS_FOUND = "NO_MATCHING_SUB_STATEMENTS_FOUND",
}
