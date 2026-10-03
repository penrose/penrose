export interface SourceToken {
  text: string;
  kind?: "comment" | "string" | "keyword" | "number";
}

/** Small lexical highlighter; Vue text nodes preserve and escape the actual program. */
export function sourceTokens(source: string): SourceToken[] {
  const tokens: SourceToken[] = [];
  const pattern =
    /\/\/[^\n]*|\/\*[\s\S]*?\*\/|"(?:\\[\s\S]|[^"\\])*"|'(?:\\[\s\S]|[^'\\])*'|`(?:\\[\s\S]|[^`\\])*`|\b(?:as|async|await|break|case|catch|class|const|continue|declare|default|delete|else|export|extends|false|finally|for|from|function|if|import|in|instanceof|interface|let|new|null|of|readonly|return|satisfies|static|super|switch|this|throw|true|try|type|typeof|undefined|void|while|yield)\b|\b(?:0[xX][\da-fA-F]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)\b/g;
  let cursor = 0;
  for (const match of source.matchAll(pattern)) {
    const start = match.index!;
    if (start > cursor) tokens.push({ text: source.slice(cursor, start) });
    const text = match[0];
    const kind =
      text.startsWith("//") || text.startsWith("/*")
        ? "comment"
        : /^['"`]/.test(text)
        ? "string"
        : /^\d/.test(text)
        ? "number"
        : "keyword";
    tokens.push({ text, kind });
    cursor = start + text.length;
  }
  if (cursor < source.length) tokens.push({ text: source.slice(cursor) });
  return tokens;
}
