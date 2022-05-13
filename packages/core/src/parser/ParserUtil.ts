import { compact, flatten } from "lodash";
import * as moo from "moo";
import { C, Identifier, NodeType, SourceLoc, SourceRange } from "types/ast";

export const basicSymbols: moo.Rules = {
  ws: /[ \t]+/,
  nl: { match: /\r\n|\r|\n/, lineBreaks: true },
  subtypeOf: "<:",
  lte: "<=",
  lt: "<",
  gte: ">=",
  gt: ">",
  eq: "==",
  rarrow: "->",
  tilda: "~",
  lparen: "(",
  rparen: ")",
  apos: "'",
  comma: ",",
  string_literal: {
    // match: /"(?:[^\n\\"]|\\["\\ntbfr])*"/,
    // Not sure why we were disallowing backslashes in string literals;
    // these are needed to write TeX inline in Style (e.g., as the string
    // field for an Equation shape).
    match: /"(?:[^\n"]|\\["\\ntbfr])*"/,
    value: (s: string): string => s.slice(1, -1),
  },
  float_literal: /[+-]?(?:\d+(?:[.]\d*)?(?:[eE][+-]?\d+)?|[.]\d+(?:[eE][+-]?\d+)?)/,
  comment: /--.*?$/,
  multiline_comment: {
    match: /\/\*(?:[\s\S]*?)\*\//,
    lineBreaks: true,
  },
  dot: ".",
  brackets: "[]",
  lbracket: "[",
  rbracket: "]",
  lbrace: "{",
  rbrace: "}",
  assignment: "=",
  def: ":=",
  plus: "+",
  exp: "^",
  minus: "-",
  multiply: "*",
  divide: "/",
  modulo: "%",
  colon: ":",
  semi: ";",
  question: "?",
  dollar: "$",
  tick: "`",
};

const tokenStart = (token: moo.Token): SourceLoc => {
  return {
    line: token.line,
    col: token.col - 1,
  };
};

const tokenEnd = (token: moo.Token): SourceLoc => {
  const { text, lineBreaks } = token;
  let { line, col } = token;
  col += text.length - 1;
  if (lineBreaks) {
    const lines = text.split(/\r\n|\r|\n/);
    const last = lines.length - 1;
    line += last;
    col = lines[last].length;
  }
  return { line, col };
};

export const rangeOf = (token: moo.Token | SourceRange): SourceRange => {
  // if it's already converted, assume it's an AST Node
  if ("start" in token && "end" in token) {
    return { start: token.start, end: token.end };
  }
  return {
    start: tokenStart(token),
    end: tokenEnd(token),
  };
};

const before = (loc1: SourceLoc, loc2: SourceLoc) => {
  if (loc1.line < loc2.line) {
    return true;
  }
  if (loc1.line > loc2.line) {
    return false;
  }
  return loc1.col < loc2.col;
};

const minLoc = (...locs: SourceLoc[]) => {
  if (locs.length === 0) throw new TypeError();
  let minLoc = locs[0];
  for (const l of locs) if (before(l, minLoc)) minLoc = l;
  return minLoc;
};

const maxLoc = (...locs: SourceLoc[]) => {
  if (locs.length === 0) throw new TypeError();
  let maxLoc = locs[0];
  for (const l of locs) if (before(maxLoc, l)) maxLoc = l;
  return maxLoc;
};

/** Given a list of nodes, find the range of nodes */
export const rangeFrom = (children: SourceRange[]): SourceRange => {
  // NOTE: this function is called in intermediate steps with empty lists, so will need to guard against empty lists.
  if (children.length === 0) {
    // console.trace(`No children ${JSON.stringify(children)}`);
    return { start: { line: 1, col: 1 }, end: { line: 1, col: 1 } };
  }

  if (children.length === 1) {
    const child = children[0];
    return { start: child.start, end: child.end };
  }

  // NOTE: use of compact to remove optional nodes
  return {
    start: minLoc(...compact(children).map((n) => n.start)),
    end: maxLoc(...compact(children).map((n) => n.end)),
    // children TODO: decide if want children/parent pointers in the tree
  };
};

export const rangeBetween = (
  // alternatively, beginToken could be undefined if endToken isn't
  beginToken: moo.Token | SourceRange,
  endToken: moo.Token | SourceRange | undefined
): SourceRange => {
  // handle undefined cases for easier use in postprocessors
  if (!endToken) return rangeOf(beginToken);
  if (!beginToken) return rangeOf(endToken);
  const [beginRange, endRange] = [beginToken, endToken].map(rangeOf);
  return {
    start: beginRange.start,
    end: endRange.end,
  };
};

// const walkTree = <T>(root: ASTNode, f: (ASTNode): T) => {
// TODO: implement
// }

export const convertTokenId = ([token]: moo.Token[]): Partial<
  Identifier<C>
> => {
  return {
    ...rangeOf(token),
    value: token.text,
    type: token.type,
  };
};

export const nth = <T>(n: number) => {
  return function (d: T[]): T {
    return d[n];
  };
};

export const optional = <T>(optionalValue: T | undefined, defaultValue: T): T =>
  optionalValue ? optionalValue : defaultValue;
// Helper that takes in a mix of single token or list of tokens, drops all undefined (i.e. optional ealues), and finally flattten the mixture to a list of tokens.
export const tokensIn = (
  tokenList: (moo.Token | moo.Token[] | undefined)[]
): moo.Token[] => flatten(compact(tokenList));

// HACK: locations for dummy AST nodes. Revisit if this pattern becomes widespread.
export const idOf = (value: string, nodeType: NodeType): Identifier<C> => ({
  nodeType,
  start: { line: 1, col: 1 },
  end: { line: 1, col: 1 },
  tag: "Identifier",
  type: "identifier",
  value,
});

export const lastLocation = (parser: nearley.Parser): SourceLoc | undefined => {
  const lexerState = parser.lexerState;
  if (lexerState) {
    return {
      line: lexerState.line,
      col: lexerState.col,
    };
  } else {
    return undefined;
  }
};
