import katex from "katex";
import markdownItKatex from "markdown-it-katex";

/** Keep the existing dollar-delimiter parser, with the site's matching KaTeX. */
export function configureMath(md) {
  md.use(markdownItKatex);
  const render = (source, displayMode) =>
    katex.renderToString(source, {
      displayMode,
      output: "htmlAndMathml",
      throwOnError: true,
      strict: "error",
    });
  // The plugin bundles KaTeX 0.6 and silently emits raw input on parse failure.
  // Render with the direct dependency instead, and fail on invalid mathematics.
  md.renderer.rules.math_inline = (tokens, index) =>
    `<span v-pre>${render(tokens[index].content, false)}</span>`;
  md.renderer.rules.math_block = (tokens, index) =>
    `<div v-pre>${render(tokens[index].content, true)}</div>\n`;
}
