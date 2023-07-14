import markdownItKatex from "markdown-it-katex";
import { defineConfig } from "vitepress";
import domainGrammar from "../../vscode/syntaxes/domain.tmGrammar.json";
import styleGrammar from "../../vscode/syntaxes/style.tmGrammar.json";
import substanceGrammar from "../../vscode/syntaxes/substance.tmGrammar.json";

const styleLang = {
  id: "style",
  scopeName: "source.penrose-style",
  grammar: styleGrammar,
  path: "style.tmGrammar.json",
};
const domainLang = {
  id: "domain",
  scopeName: "source.penrose-domain",
  grammar: domainGrammar,
  path: "domain.tmGrammar.json",
};
const substanceLang = {
  id: "substance",
  scopeName: "source.penrose-substance",
  grammar: substanceGrammar,
  path: "substance.tmGrammar.json",
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
    // TODO: figure out the current types of language configs
    languages: [substanceLang as any, domainLang as any, styleLang as any],
  },
  vue: {
    template: {
      compilerOptions: {
        isCustomElement: (tag) => customElements.includes(tag),
      },
    },
  },

  themeConfig: {
    logo: "img/favicon.ico",
    outline: "deep",
    nav: [
      {
        text: "Examples",
        link: "/examples",
        activeMatch: "/examples",
      },
      {
        text: "Learn Penrose",
        link: "/docs/tutorial/welcome",
        activeMatch: "/docs/tutorial",
      },
      { text: "Documentation", link: "/docs/ref", activeMatch: "/docs/ref" },
      { text: "Try Penrose", link: "pathname:///try/index.html" },
      { text: "Join Discord", link: "https://discord.gg/Y3K2kHxp2b" },
      { text: "Team", link: "/docs/team" },
      { text: "Blog", link: "/blog", activeMatch: "/blog" },
      {
        text: "News",
        items: [
          { text: "SIGGRAPH'20 paper", link: "pathname:///siggraph20.html" },
          {
            text: "CHI'20 paper",
            link: "https://www.cs.cmu.edu/~woden/assets/chi-20-natural-diagramming.pdf",
          },
          {
            text: "Popular Mechanics",
            link: "https://www.popularmechanics.com/science/math/a32743509/cmu-penrose-math-equations-into-pictures/",
          },
        ],
      },
    ],

    socialLinks: [
      { icon: "github", link: "https://github.com/penrose/penrose" },
      { icon: "twitter", link: "https://twitter.com/UsePenrose" },
      { icon: "discord", link: "https://discord.gg/Y3K2kHxp2b" },
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
              items: [
                { text: "Usage", link: "/docs/ref/domain/usage" },
                { text: "Examples", link: "/docs/ref/domain/examples" },
              ],
            },
            {
              text: "Substance",
              items: [
                { text: "Usage", link: "/docs/ref/substance/usage" },
                { text: "Examples", link: "/docs/ref/substance/examples" },
              ],
            },
            {
              text: "Style",
              items: [
                {
                  text: "Usage",
                  link: "/docs/ref/style/usage",
                },
                {
                  text: "Value Types",
                  link: "/docs/ref/style/value-types",
                },
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
                { text: "Function Library", link: "/docs/ref/style/functions" },
                {
                  text: "Passthrough SVG",
                  link: "/docs/ref/style/passthrough",
                },
                { text: "Examples", link: "/docs/ref/style/examples" },
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
