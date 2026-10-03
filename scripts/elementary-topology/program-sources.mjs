import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import path from "node:path";
import ts from "typescript";

/** Resolve source ranges from declarations rather than maintaining copies of programs. */
export async function substanceSourceLines(root, figure) {
  const filename = figure.substanceModule ?? figure.implementation;
  assert(
    filename && figure.substanceFactory,
    `Missing Substance provenance for ${figure.id}`,
  );
  const source = ts.createSourceFile(
    filename,
    await readFile(path.join(root, filename), "utf8"),
    ts.ScriptTarget.Latest,
    true,
  );
  let declaration;
  const visit = (node) => {
    if (
      (ts.isFunctionDeclaration(node) &&
        node.name?.text === figure.substanceFactory) ||
      (ts.isVariableStatement(node) &&
        node.declarationList.declarations.some(
          (d) => d.name.getText(source) === figure.substanceFactory,
        ))
    )
      declaration = node;
    ts.forEachChild(node, visit);
  };
  visit(source);
  assert(
    declaration,
    `Missing Substance declaration ${figure.substanceFactory} for ${figure.id}`,
  );
  return [
    source.getLineAndCharacterOfPosition(declaration.getStart(source)).line + 1,
    source.getLineAndCharacterOfPosition(declaration.getEnd()).line + 1,
  ];
}
