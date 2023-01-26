// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import * as nearley from "nearley";
import grammar from "./StyleParser";

const tokenTypes = [
  "class",
  "interface",
  "enum",
  "struct",
  "typeParameter",
  "type",
  "parameter",
  "variable",
  "property",
  "enumMember",
  "method",
  "number",
  "operator",
  "string",
  "comment",
  "label",
];

const tokenModifiers = ["static", "readonly", "declaration", "definition"];

const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

// this method is called when your extension is deactivated
export function deactivate() {}

class DocumentSemanticTokensProvider
  implements vscode.DocumentSemanticTokensProvider {
  async provideDocumentSemanticTokens(
    document: vscode.TextDocument,
    token: vscode.CancellationToken
  ): Promise<vscode.SemanticTokens> {
    const tokensBuilder = new vscode.SemanticTokensBuilder(legend);
    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
    const { results } = parser.feed(document.getText());
    return tokensBuilder.build();
  }
}

const selector = { language: "penrose-style", scheme: "file" };

export function activate(context: vscode.ExtensionContext) {
  console.log("activate");
  context.subscriptions.push(
    vscode.languages.registerDocumentSemanticTokensProvider(
      selector,
      new DocumentSemanticTokensProvider(),
      legend
    )
  );
}
