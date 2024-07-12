import { compDict, constrDict, describeType, objDict } from "@penrose/core";
import markdownItKatex from "markdown-it-katex";
import { defineConfig } from "vitepress";
import domainGrammar from "../../vscode/syntaxes/domain.tmGrammar.json";
import styleGrammar from "../../vscode/syntaxes/style.tmGrammar.json";
import substanceGrammar from "../../vscode/syntaxes/substance.tmGrammar.json";

const styleLang = {
  name: "style",
  scopeName: "source.penrose-style",
  repository: styleGrammar.repository as any,
  patterns: styleGrammar.patterns,
};
const domainLang = {
  name: "domain",
  scopeName: "source.penrose-domain",
  repository: domainGrammar.repository as any,
  patterns: domainGrammar.patterns,
};
const substanceLang = {
  name: "substance",
  scopeName: "source.penrose-substance",
  repository: substanceGrammar.repository as any,
  patterns: substanceGrammar.patterns,
};

// generate a markdown string from computation, objective, and constraint dictionaries.
// specifically, the anchors in this markdown string is used to generate previews and the search result links
const indexableFunctionDocs = () => {
  const showParams = (ps) =>
    ps.map((p) => `${p.name}: ${p.description}`).join("\n\n");
  const showReturn = (r) => {
    const t = describeType(r);
    return `${t.symbol}: ${t.description}`;
  };
  const compFuncs = Object.entries(compDict).map(([k, v]: any) => {
    return `### ${v.name} {#computation-${v.name}}\n\n${
      v.description
    }\n\n**Returns:** ${showReturn(
      v.returns,
    )}\n\n**Parameters:**\n\n${showParams(v.params)}`;
  });
  const objectives = Object.entries(objDict).map(([k, v]: any) => {
    return `### ${v.name} {#objective-${v.name}}\n\n${
      v.description
    }\n\n**Parameters:**\n\n${showParams(v.params)}`;
  });
  const constraints = Object.entries(constrDict).map(([k, v]: any) => {
    return `### ${v.name} {#constraint-${v.name.toLowerCase()}}\n\n${
      v.description
    }\n\n**Parameters:**\n\n${showParams(v.params)}`;
  });
  const markdown = [
    "## Constraints\n\n",
    ...constraints,
    "## Objectives\n\n",
    ...objectives,
    "## Computation\nn",
    ...compFuncs,
  ].join("\n\n");

  return markdown;
};

// https://github.com/vuejs/vitepress/issues/529#issuecomment-1151186631
const customElements = [
  "math",
  "maction",
  "maligngroup",
  "malignmark",
  "menclose",
  "merror",
  "mfenced",
  "mfrac",
  "mi",
  "mlongdiv",
  "mmultiscripts",
  "mn",
  "mo",
  "mover",
  "mpadded",
  "mphantom",
  "mroot",
  "mrow",
  "ms",
  "mscarries",
  "mscarry",
  "mscarries",
  "msgroup",
  "mstack",
  "mlongdiv",
  "msline",
  "mstack",
  "mspace",
  "msqrt",
  "msrow",
  "mstack",
  "mstack",
  "mstyle",
  "msub",
  "msup",
  "msubsup",
  "mtable",
  "mtd",
  "mtext",
  "mtr",
  "munder",
  "munderover",
  "semantics",
  "math",
  "mi",
  "mn",
  "mo",
  "ms",
  "mspace",
  "mtext",
  "menclose",
  "merror",
  "mfenced",
  "mfrac",
  "mpadded",
  "mphantom",
  "mroot",
  "mrow",
  "msqrt",
  "mstyle",
  "mmultiscripts",
  "mover",
  "mprescripts",
  "msub",
  "msubsup",
  "msup",
  "munder",
  "munderover",
  "none",
  "maligngroup",
  "malignmark",
  "mtable",
  "mtd",
  "mtr",
  "mlongdiv",
  "mscarries",
  "mscarry",
  "msgroup",
  "msline",
  "msrow",
  "mstack",
  "maction",
  "semantics",
  "annotation",
  "annotation-xml",
];

export default defineConfig({
  title: "Penrose",
  description:
    "Create beautiful diagrams just by typing math notation in plain text.",

  cleanUrls: true,
  ignoreDeadLinks: true,
  outDir: "build",

  head: [
    ["link", { rel: "icon", href: "/img/logo.svg" }],
    [
      "link",
      {
        rel: "stylesheet",
        href: "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css",
      },
    ],
  ],

  markdown: {
    config: (md) => {
      md.use(markdownItKatex);
    },
    languages: [styleLang, domainLang, substanceLang],
  },
  vue: {
    template: {
      compilerOptions: {
        isCustomElement: (tag) => customElements.includes(tag),
      },
    },
  },

  themeConfig: {
    search: {
      provider: "local",
      options: {
        _render(src, env, md) {
          // hijack the render function before the search engine indexes the page
          const html = md.render(src, env);
          // this is a hack to add anchors to the markdown file for function documentation
          if (env.frontmatter?.anchors === "functions") {
            return md.render(indexableFunctionDocs()) + html;
          }
          return html;
        },
      },
    },
    logo: "img/favicon.ico",
    outline: "deep",
    editLink: {
      pattern:
        "https://github.com/penrose/penrose/edit/main/packages/docs-site/:path",
    },
    nav: [
      {
        text: "Examples",
        link: "/examples",
        activeMatch: "/examples",
      },
      {
        text: "Contribute",
        link: "/community",
        activeMatch: "/community",
      },
      {
        text: "Learn",
        link: "/docs/tutorial/welcome",
        activeMatch: "/docs/tutorial",
      },
      { text: "Docs", link: "/docs/ref", activeMatch: "/docs/ref" },
      { text: "Blog", link: "/blog", activeMatch: "/blog" },
      { text: "Team", link: "/docs/team" },
      { text: "Editor", link: "/try/index.html", target: "_blank" },
      { text: "Join", link: "https://discord.gg/a7VXJU4dfR" },
      //   {
      //     text: "News",
      //     items: [
      //       { text: "SIGGRAPH'20 paper", link: "pathname:///siggraph20.html" },
      //       {
      //         text: "CHI'20 paper",
      //         link: "https://www.cs.cmu.edu/~woden/assets/chi-20-natural-diagramming.pdf",
      //       },
      //       {
      //         text: "Popular Mechanics",
      //         link: "https://www.popularmechanics.com/science/math/a32743509/cmu-penrose-math-equations-into-pictures/",
      //       },
      //     ],
      //   },
    ],

    socialLinks: [
      { icon: "github", link: "https://github.com/penrose/penrose" },
      { icon: "twitter", link: "https://twitter.com/UsePenrose" },
      { icon: "discord", link: "https://discord.gg/a7VXJU4dfR" },
    ],

    sidebar: {
      "/docs/tutorial": [
        {
          text: "Tutorial",
          items: [
            { text: "Welcome!", link: "/docs/tutorial/welcome" },
            { text: "Basics", link: "/docs/tutorial/basics" },
            {
              text: "Predicates & Constraints",
              link: "/docs/tutorial/predicates",
            },
            { text: "Functions", link: "/docs/tutorial/functions" },
          ],
        },
      ],
      "/docs/ref": [
        {
          text: "Reference",
          items: [
            { text: "Overview", link: "/docs/ref" },
            { text: "Using Penrose", link: "/docs/ref/using" },
            {
              text: "Domain",
              link: "/docs/ref/domain/overview",
              items: [
                { text: "Types", link: "/docs/ref/domain/types" },
                { text: "Predicates", link: "/docs/ref/domain/predicates" },
                {
                  text: "Functions and Constructors",
                  link: "/docs/ref/domain/functions",
                },
              ],
            },
            {
              text: "Substance",
              link: "/docs/ref/substance/overview",
              items: [
                {
                  text: "Statements",
                  link: "/docs/ref/substance/statements",
                },
                {
                  text: "Indexed Statements",
                  link: "/docs/ref/substance/indexed-statements",
                },
                {
                  text: "Literal Expressions",
                  link: "/docs/ref/substance/literal-expressions",
                },
              ],
            },
            {
              text: "Style",
              link: "/docs/ref/style/overview",
              items: [
                {
                  text: "Namespaces",
                  link: "/docs/ref/style/namespaces",
                },
                {
                  text: "Selectors",
                  link: "/docs/ref/style/selectors",
                },
                {
                  text: "Selector Blocks",
                  link: "/docs/ref/style/selector-blocks",
                },
                {
                  text: "Collectors",
                  link: "/docs/ref/style/collectors",
                },
                {
                  text: "Literals",
                  link: "/docs/ref/style/literals",
                },
                {
                  text: "Expressions",
                  link: "/docs/ref/style/expressions",
                },
                {
                  text: "Value Types",
                  link: "/docs/ref/style/value-types",
                },
                {
                  text: "Vectors and Matrices",
                  link: "/docs/ref/style/vectors-matrices",
                },
                { text: "Function Library", link: "/docs/ref/style/functions" },
                {
                  text: "Shapes",
                  link: "/docs/ref/style/shapes-overview",
                  items: [
                    // Please make sure the shapes are in alphabetical order.
                    { text: "Circle", link: "/docs/ref/style/shapes/circle" },
                    { text: "Ellipse", link: "/docs/ref/style/shapes/ellipse" },
                    {
                      text: "Equation",
                      link: "/docs/ref/style/shapes/equation",
                    },
                    { text: "Group", link: "/docs/ref/style/shapes/group" },
                    { text: "Image", link: "/docs/ref/style/shapes/image" },
                    { text: "Line", link: "/docs/ref/style/shapes/line" },
                    { text: "Path", link: "/docs/ref/style/shapes/path" },
                    { text: "Polygon", link: "/docs/ref/style/shapes/polygon" },
                    {
                      text: "Polyline",
                      link: "/docs/ref/style/shapes/polyline",
                    },
                    {
                      text: "Rectangle",
                      link: "/docs/ref/style/shapes/rectangle",
                    },
                    { text: "Text", link: "/docs/ref/style/shapes/text" },
                  ],
                },
                {
                  text: "Random Sampling",
                  link: "/docs/ref/style/random-sampling",
                },
                {
                  text: "Passthrough SVG",
                  link: "/docs/ref/style/passthrough",
                },
              ],
            },
          ],
        },
        {
          text: "For Developers",
          items: [
            {
              text: "The Language API",
              link: "/docs/ref/api",
            },
            {
              text: "The Optimization API",
              link: "/docs/ref/optimization-api",
            },
            {
              text: "Using Penrose with Vanilla JS",
              link: "/docs/ref/vanilla-js",
            },
            {
              text: "Using Penrose with a Bundler",
              link: "/docs/ref/bundle",
            },
            {
              text: "Using Penrose with React",
              link: "/docs/ref/react",
            },
            {
              text: "Using Penrose with SolidJS",
              link: "/docs/ref/solid",
            },
            {
              text: "Writing Constraints & Objectives",
              link: "/docs/ref/constraints",
            },
          ],
        },
      ],
      "/blog": [
        {
          text: "August 2023",
          items: [
            {
              text: "Tailoring Penrose domains to your needs",
              link: "/blog/tailoring-graph-domain",
            },
          ],
        },
        {
          text: "July 2023",
          items: [
            {
              text: "Announcing Penrose 3.0",
              link: "/blog/v3",
            },
          ],
        },
        {
          text: "June 2023",
          items: [
            {
              text: "Diagram Layout in Stages",
              link: "/blog/staged-layout",
            },
            {
              text: "What Have We Done to the Languages?",
              link: "/blog/new-language-features",
            },
            {
              text: "Switching to Wasm for 10x Speedup",
              link: "/blog/wasm",
            },
          ],
        },
      ],
    },

    footer: {
      message:
        'Released under the <a href="https://github.com/penrose/penrose/blob/main/LICENSE">MIT License</a>.',
      copyright: "Copyright © 2017-present Penrose contributors",
    },
  },
});
