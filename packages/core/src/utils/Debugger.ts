import { A } from "types/ast";
import { DomainProg } from "types/domain";
import { RelationPatternSubst, Selector, StyProg } from "types/style";
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
export class Debugger<T> {
  private state = 0; // 0=listening, 1=answering
  private static theInstance: Debugger<A>;
  private rep: DebugStyleBlock<T>[] = [];
  private domSrc = "";
  private subSrc = "";
  private stySrc = "";
  private domAst?: DomainProg<T>;
  private subAst?: SubProg<T>;
  private styAst?: StyProg<T>;

  // ------------------------- Singleton Impl. -----------------------------//

  /**
   * Replaces the singleton debugger with a new instance, i.e., for when
   * a new program is being compiled/debugged.
   *
   * @returns A new instance of the debugger
   */
  public static newInstance(): Debugger<A> {
    this.theInstance = new Debugger();
    return this.theInstance;
  }
  /**
   * Returns the current instance of the debugger.
   *
   * @returns The current instance of the debugger
   */
  public static getInstance(): Debugger<A> {
    if (this.theInstance == undefined) {
      return Debugger.newInstance();
    } else {
      return this.theInstance;
    }
  }

  // ------------------------ Setters / Getters ----------------------------//

  public addBlock(block: DebugStyleBlock<T>): void {
    this.moveToListeningState();
    this.rep.push(JSON.parse(JSON.stringify(block)));
  }
  public getBlocks(): DebugStyleBlock<T>[] {
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
  public setDomAst(domAst: DomainProg<T>): void {
    this.moveToListeningState();
    this.domAst = JSON.parse(JSON.stringify(domAst)); // Protect the rep
  }
  public setSubAst(subAst: SubProg<T>): void {
    this.moveToListeningState();
    this.subAst = JSON.parse(JSON.stringify(subAst)); // Protect the rep
  }
  public setStyAst(styAst: StyProg<T>): void {
    this.moveToListeningState();
    this.styAst = JSON.parse(JSON.stringify(styAst)); // Protect the rep
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
  ): DebugStyleBlockRel<T>[] {
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
              theBlock as DebugStyleBlock<unknown>,
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
      /*
      this.srcMap = mapShapesToSource(
        this.domAst,
        this.domSrc,
        this.subAst,
        this.subSrc,
        this.styAst,
        this.stySrc
      );
      */
      this.addSourceToReasons();
    }
  }

  // ------------------------- Helper Functions ----------------------------//

  private addSourceToReasons() {
    this.moveToAnsweringState();
    this.rep.forEach((block) => {
      // Resolve source refs to source text
      block.unsats.concat(block.sats).forEach((unsat) => {
        unsat.reasons.forEach((reason) => {
          reason.srcRef.forEach((srcRef) => {
            srcRef.srcText = this.getSource(
              this.stySrc,
              { line: srcRef.lineStart, col: srcRef.colStart },
              { line: srcRef.lineEnd, col: srcRef.colEnd }
            );
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
    rel: DebugStyleBlockRel<unknown>,
    block: DebugStyleBlock<unknown>,
    relVars: Subst
  ): boolean => {
    // Throw an exception if provided an empty query
    if (relVars == {}) {
      throw new Error(`Debug query has no Style variable substitution`);
    }

    // Loop over the substitution pairs in the query
    for (const k in relVars) {
      // If the queried style variable exists in the block...
      if (k in rel.rel.subst) {
        // ... and the bound Substance variables do not match, return false
        if (relVars[k] !== rel.rel.subst[k]) {
          return false;
        }
      } else {
        // ... otherwise throw an exception if the style variable does not exist
        throw new Error(
          `Style variable '${relVars[k]}' not found in Style block at lines ${block.sel["start"].line}-${block.sel["end"].line}`
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
  private getBlockAtLineNumber(
    styLine: number
  ): DebugStyleBlock<T> | undefined {
    // Loop over each style block to find the one specified on input
    for (const i in this.rep) {
      const block = this.rep[i];
      // Find the block specified by the line number; ignore the rest
      if (
        block.sel["start"].line <= styLine &&
        block.sel["end"].line >= styLine
      ) {
        return block;
      }
    }
    return undefined;
  }

  /**
   * Returns source text for the given start and end positions
   * @param pgmSrc Program source
   * @param start Start line and column {line: number, col: number}
   * @param end End line and column {line: number, col: number}
   * @returns Source text
   */
  private getSource(
    pgmSrc: string,
    start?: { line: number; col: number },
    end?: { line: number; col: number }
  ): string[] {
    if (start && end) {
      start.line = start.line - 1;
      end.line = end.line - 1;
      end.col = end.col + 1;
    }

    const lines = pgmSrc.split(/\r?\n/);
    const outLines: string[] = [];
    const startLine = start ? start.line : 0;
    const endLine = end ? end.line : lines.length - 1;

    for (let i = startLine; i < endLine + 1; i++) {
      if (start && end) {
        if (i >= start.line && i <= end.line) {
          if (start.line == end.line) {
            if (start.col <= end.col) {
              outLines.push(lines[i].substring(start.col, end.col));
            } else {
              outLines.push(lines[i].substring(start.col));
            }
          } else if (i == start.line) {
            outLines.push(lines[i].substring(start.col));
          } else if (i == end.line) {
            outLines.push(lines[i].substring(0, end.col));
          } else {
            outLines.push(lines[i]);
          }
        }
      } else {
        outLines.push(lines[i]);
      }
    }
    return outLines;
  }
}

export type DebugStyleBlock<T> = {
  sel: Selector<unknown>; // Selection Block
  substs: Subst[]; // List of substitutions tried
  hasWhereClause: boolean; // true = Where clause present, false otherwise
  sats: DebugStyleBlockRel<T>[]; // List of satisfied relations (if not a match-all block)
  unsats: DebugStyleBlockRel<T>[]; // List of unsatisfied relations (if not a match-all block)
};
export type DebugStyleBlockRel<T> = {
  rel: RelationPatternSubst<T>;
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
  DOMAIN = "DOM",
  SUBSTANCE = "SUB",
  STYLE = "STY",
}
export enum DebugReasonCodes {
  MATCHING_SUB_STATEMENTS_FOUND = "MATCHING_SUB_STATEMENTS_FOUND",
  NO_MATCHING_SUB_STATEMENTS = "NO_MATCHING_SUB_STATEMENTS",
}
