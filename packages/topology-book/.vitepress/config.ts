import react from "@vitejs/plugin-react";
import { fileURLToPath } from "node:url";
import topLevelAwait from "vite-plugin-top-level-await";
import { defineConfig } from "vitepress";
import { configureMath } from "../../docs-site/.vitepress/math.mjs";

const packageRoot = fileURLToPath(new URL("../", import.meta.url));
const bookHref = (href: string) => {
  const suffix = href.replace(/^\/docs\/elementary-topology(?=\/|\?|#|$)/, "");
  return suffix === href
    ? href
    : suffix.startsWith("/")
    ? suffix
    : `/${suffix}`;
};
const rewriteHtmlLinks = (html: string) =>
  html.replace(
    /href=(["'])(\/docs\/elementary-topology(?:\/[^"']*|\?[^"']*|#[^"']*)?)\1/g,
    (_match, quote, href) => `href=${quote}${bookHref(href)}${quote}`,
  );

export default defineConfig({
  title: "Elementary Topology",
  description:
    "Michael C. Gemignani's Elementary Topology, second edition, with interactive mathematical illustrations.",
  srcDir: "../docs-site/docs/elementary-topology",
  outDir: ".vitepress/dist",
  cleanUrls: true,
  lastUpdated: false,
  head: [
    ["meta", { name: "theme-color", content: "#f7f4ee" }],
    [
      "link",
      {
        rel: "icon",
        href: "/elementary-topology/figures/figure-cover-rosette.svg",
        type: "image/svg+xml",
      },
    ],
  ],
  markdown: {
    config(md) {
      configureMath(md);
      const linkOpen =
        md.renderer.rules.link_open ??
        ((tokens, index, options, _env, renderer) =>
          renderer.renderToken(tokens, index, options));
      md.renderer.rules.link_open = (tokens, index, options, env, renderer) => {
        const href = tokens[index].attrGet("href");
        if (href) tokens[index].attrSet("href", bookHref(href));
        return linkOpen(tokens, index, options, env, renderer);
      };
      for (const rule of ["html_block", "html_inline"] as const) {
        const render = md.renderer.rules[rule];
        if (render)
          md.renderer.rules[rule] = (...args) =>
            rewriteHtmlLinks(render(...args));
      }
    },
  },
  vite: {
    publicDir: `${packageRoot}public`,
    build: { target: "esnext" },
    plugins: [topLevelAwait(), react()],
    optimizeDeps: {
      esbuildOptions: { target: "esnext" },
      exclude: ["@penrose/examples", "rose"],
    },
    server: {
      headers: {
        "Cross-Origin-Embedder-Policy": "require-corp",
        "Cross-Origin-Opener-Policy": "same-origin",
      },
    },
    preview: {
      headers: {
        "Cross-Origin-Embedder-Policy": "require-corp",
        "Cross-Origin-Opener-Policy": "same-origin",
      },
    },
  },
});
