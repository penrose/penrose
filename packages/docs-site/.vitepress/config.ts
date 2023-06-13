import markdownItKatex from "markdown-it-katex";
import { defineConfig } from "vitepress";

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
            { text: "Penrose Overview", link: "/docs/ref" },
            {
              text: "Domain",
              items: [
                { text: "Overview", link: "/docs/ref/domain" },
                { text: "Usage", link: "/docs/ref/domain/usage" },
                { text: "Examples", link: "/docs/ref/domain/examples" },
              ],
            },
            {
              text: "Substance",
              items: [
                { text: "Overview", link: "/docs/ref/substance" },
                { text: "Usage", link: "/docs/ref/substance/usage" },
                { text: "Examples", link: "/docs/ref/substance/examples" },
              ],
            },
            {
              text: "Style",
              items: [
                { text: "Overview", link: "/docs/ref/style" },
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
              text: "What Have We Done to the Languages?",
              link: "/blog/2023/06/06/new-language-features",
            },
            {
              text: "Porting the Penrose Optimizer from TypeScript to Rust and WebAssembly for 10x Speedup",
              link: "/blog/2023/06/06/wasm",
            },
          ],
        },
      ],
    },

    footer: { copyright: "made with ❤️ in Pittsburgh and abroad" },
  },
});
