import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";

// helpful resources:
// - https://github.com/cevek/ttypescript/tree/v1.5.5#transformers
// - https://github.com/itsdouges/typescript-transformer-handbook/tree/e34bd900c97cb688e99ae074da14b50131171ad5#adding-new-import-declarations

// keep in sync with `./index.d.ts`
const extensions = new Set([".domain", ".style", ".substance", ".svg"]);

// keep in sync with `./tsconfig.json`
const root = __dirname;
const src = path.join(root, "src");
const dist = path.join(root, "dist");

// anytime we see an import using one of the `extensions` specified above, write
// out a JSON-ified version of that file and replace the import
export default () => {
  // keep track of the files we've already seen
  const paths = new Set<string>();
  return (ctx: ts.TransformationContext) => {
    const { factory } = ctx;
    return (sourceFile: ts.SourceFile) => {
      const prefix = path.relative(src, sourceFile.fileName);
      const visitor = (node: ts.Node): ts.Node => {
        if (ts.isImportDeclaration(node)) {
          const { moduleSpecifier } = node;
          if (ts.isStringLiteral(moduleSpecifier)) {
            const { text } = moduleSpecifier;
            if (extensions.has(path.extname(text))) {
              const p = path.join(path.dirname(prefix), text);
              // don't waste time JSON-ifying a file we've seen already
              if (!paths.has(p)) {
                paths.add(p);
                const q = path.join(dist, `${p}.json`);
                fs.mkdirSync(path.dirname(q), { recursive: true });
                fs.writeFileSync(
                  q,
                  JSON.stringify(fs.readFileSync(path.join(src, p), "utf8"))
                );
              }
              return factory.createImportDeclaration(
                node.modifiers,
                node.importClause,
                factory.createStringLiteral(`${text}.json`)
              );
            }
          }
        }
        return ts.visitEachChild(node, visitor, ctx);
      };
      return ts.visitEachChild(sourceFile, visitor, ctx);
    };
  };
};
