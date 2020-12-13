// const grammar = require("./Style.ne");
import * as nearley from "nearley";
import grammar from "./StyleParser";

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const sameASTs = (results: any[]) => {
  for (const p of results) expect(results[0]).toEqual(p);
  expect(results.length).toEqual(1);
};

test("empty program", () => {
  const { results } = parser.feed("");
  sameASTs(results);
  expect(results.length).toBe(1);
});

test("empty namespace block", () => {
  const { results } = parser.feed("const { }");
  sameASTs(results);
  expect(results.length).toBe(1);
});

test("empty selector block", () => {
  const { results } = parser.feed("forall Set A{}");
  sameASTs(results);
});

test("comment and empty blocks", () => {
  const prog = `
  -- this is a comment

  Set A with Set B { 

  }
  
  forall Set A, \`B\`; Map f 
  { 
  }`;
  const { results } = parser.feed(prog);
  sameASTs(results);
});

test("forall keyword", () => {
  const prog = `
  forall Set A, B { }

  Set A, \`B\`; Map f
  {

  }`;
  const { results } = parser.feed(prog);
  sameASTs(results);
});

test("nested preds", () => {
  const prog = `
forall Set A, B, C
where IsSubset(Union(A,B), C, Intersection(B, C));
{ }`;
  const { results } = parser.feed(prog);
  sameASTs(results);
});

test("empty pred", () => {
  const prog = `
forall Set A, B, C
where IsSubset(   );
{ }`;
  const { results } = parser.feed(prog);
  sameASTs(results);
});

test("with, where, and as", () => {
  const prog = `
forall Set A, B; Map f
with Set C, D; Map \`g\`
where C := intersect ( A, B, Not(f) ) ; IsSubset( A, B ); IsSubset( Union(A,B), C); Intersect (   )
as Const
{ }`;
  const { results } = parser.feed(prog);
  sameASTs(results);
});

// parser.feed(prog);
// console.log(JSON.stringify(parser.results[0]));
