import * as nearley from "nearley";
import styleGrammer from "./StyleParser";

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(styleGrammer));

const prog = `
-- this is a comment
-- forall Set A, B { }
forall Set A, \`B\`; Map f 
with Set C, D; Map g
as Const
{ }
-- const { }
`;

parser.feed(prog);
console.log(JSON.stringify(parser.results[0]));
