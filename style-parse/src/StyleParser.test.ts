// const grammar = require("./Style.ne");
import * as nearley from "nearley";
import grammar from "./StyleParser";

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

test("empty program", () => {
  parser.feed("");
});

test("empty block", () => {
  parser.feed("const { }");
});

test("comment and empty blocks", () => {
  const prog = `
  -- this is a comment
  forall Set A, B { }
  
  forall Set A, \`B\`; Map f 
  { 

  }`;
  parser.feed(prog);
});

test("forall keyword", () => {
  const prog = `
  forall Set A, B { }

  Set A, \`B\`; Map f
  {

  }`;
  parser.feed(prog);
});

test("with, where, and as", () => {
  const prog = `
forall Set A, B; Map f
with Set C, D; Map \`g\`
where C := intersect ( A, B, Not(f) ) ; IsSubset( A, B ); IsSubset( Union(A,B), C); Intersect (    )
as Const
{ }`;
  parser.feed(prog);
});

// parser.feed(prog);
// console.log(JSON.stringify(parser.results[0]));
