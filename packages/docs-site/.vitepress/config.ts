import { compDict, constrDict, describeType, objDict } from "@penrose/core";
import { defineConfig } from "vitepress";
import domainGrammar from "../../vscode/syntaxes/domain.tmGrammar.json";
import styleGrammar from "../../vscode/syntaxes/style.tmGrammar.json";
import substanceGrammar from "../../vscode/syntaxes/substance.tmGrammar.json";
import { configureMath } from "./math.mjs";

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
    return `### ${v.name} {#constraint-${v.name}}\n\n${
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
export const customElements = [
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

  head: [["link", { rel: "icon", href: "/img/logo.svg" }]],

  markdown: {
    config: (md) => {
      configureMath(md);
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
      {
        text: "Bloom",
        link: "/docs/bloom/tutorial/getting_started",
        activeMatch: "/docs/bloom",
      },
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
      "/docs/elementary-topology": [
        {
          text: "Elementary Topology",
          items: [
            { text: "Book index", link: "/docs/elementary-topology/" },
            { text: "Book reader", link: "/docs/elementary-topology/reader" },
            {
              text: "Front Matter",
              link: "/docs/elementary-topology/front-matter/",
              items: [
                {
                  text: "Title and Dedication",
                  link: "/docs/elementary-topology/front-matter/publication-and-dedication",
                },
                {
                  text: "Preface",
                  link: "/docs/elementary-topology/front-matter/preface",
                },
                {
                  text: "Contents",
                  link: "/docs/elementary-topology/front-matter/contents",
                },
              ],
            },
            {
              text: "1 · Preliminaries",
              link: "/docs/elementary-topology/chapter-01/",
              items: [
                {
                  text: "Sets and Functions",
                  link: "/docs/elementary-topology/chapter-01/sets-and-functions",
                },
                {
                  text: "Orderings; Equivalence Relations",
                  link: "/docs/elementary-topology/chapter-01/orderings-equivalence-relations",
                },
                {
                  text: "Cardinality",
                  link: "/docs/elementary-topology/chapter-01/cardinality",
                },
                {
                  text: "Groups",
                  link: "/docs/elementary-topology/chapter-01/groups",
                },
              ],
            },
            {
              text: "2 · Metric Spaces",
              link: "/docs/elementary-topology/chapter-02/",
              items: [
                {
                  text: "The Notion of a Metric Space",
                  link: "/docs/elementary-topology/chapter-02/metric-space",
                },
                {
                  text: "Neighborhoods",
                  link: "/docs/elementary-topology/neighborhoods",
                },
                {
                  text: "Open Sets",
                  link: "/docs/elementary-topology/chapter-02/open-sets",
                },
                {
                  text: "Closed Sets",
                  link: "/docs/elementary-topology/chapter-02/closed-sets",
                },
                {
                  text: "Convergence of Sequences",
                  link: "/docs/elementary-topology/chapter-02/convergence-of-sequences",
                },
                {
                  text: "Continuity",
                  link: "/docs/elementary-topology/chapter-02/continuity",
                },
                {
                  text: "Distance Between Two Sets",
                  link: "/docs/elementary-topology/chapter-02/distance-between-sets",
                },
              ],
            },
            {
              text: "3 · Topologies",
              link: "/docs/elementary-topology/chapter-03/",
              items: [
                {
                  text: "The Notion of a Topology",
                  link: "/docs/elementary-topology/chapter-03/topology",
                },
                {
                  text: "Bases and Subbases",
                  link: "/docs/elementary-topology/chapter-03/bases-and-subbases",
                },
                {
                  text: "Open Neighborhood Systems",
                  link: "/docs/elementary-topology/chapter-03/open-neighborhood-systems",
                },
                {
                  text: "Finer and Coarser Topologies",
                  link: "/docs/elementary-topology/chapter-03/finer-and-coarser-topologies",
                },
                {
                  text: "Derived Sets",
                  link: "/docs/elementary-topology/chapter-03/derived-sets",
                },
                {
                  text: "More About Topologically Derived Sets",
                  link: "/docs/elementary-topology/chapter-03/topologically-derived-sets",
                },
              ],
            },
            {
              text: "4 · Derived Topological Spaces. Continuity",
              link: "/docs/elementary-topology/chapter-04/",
              items: [
                {
                  text: "Subspaces",
                  link: "/docs/elementary-topology/chapter-04/subspaces",
                },
                {
                  text: "Derived Sets in Subspaces",
                  link: "/docs/elementary-topology/chapter-04/derived-sets-in-subspaces",
                },
                {
                  text: "Continuity",
                  link: "/docs/elementary-topology/chapter-04/continuity",
                },
                {
                  text: "Homeomorphisms",
                  link: "/docs/elementary-topology/chapter-04/homeomorphisms",
                },
                {
                  text: "Identification Spaces",
                  link: "/docs/elementary-topology/chapter-04/identification-spaces",
                },
                {
                  text: "Product Spaces",
                  link: "/docs/elementary-topology/chapter-04/product-spaces",
                },
              ],
            },
            {
              text: "5 · The Separation Axioms",
              link: "/docs/elementary-topology/chapter-05/",
              items: [
                {
                  text: "T₀- and T₁-Spaces",
                  link: "/docs/elementary-topology/chapter-05/t0-and-t1-spaces",
                },
                {
                  text: "T₂-Spaces",
                  link: "/docs/elementary-topology/chapter-05/t2-spaces",
                },
                {
                  text: "T₃- and Regular Spaces",
                  link: "/docs/elementary-topology/chapter-05/t3-and-regular-spaces",
                },
                {
                  text: "T₄- and Normal Spaces",
                  link: "/docs/elementary-topology/chapter-05/t4-and-normal-spaces",
                },
                {
                  text: "Normality and the Extension of Functions",
                  link: "/docs/elementary-topology/chapter-05/normality-and-extension",
                },
              ],
            },
            {
              text: "6 · Convergence",
              link: "/docs/elementary-topology/chapter-06/",
              items: [
                {
                  text: "Generalized Convergence",
                  link: "/docs/elementary-topology/chapter-06/generalized-convergence",
                },
                {
                  text: "Nets",
                  link: "/docs/elementary-topology/chapter-06/nets",
                },
                {
                  text: "Subsequences and Subnets",
                  link: "/docs/elementary-topology/chapter-06/subsequences-and-subnets",
                },
                {
                  text: "Convergence of Nets",
                  link: "/docs/elementary-topology/chapter-06/convergence-of-nets",
                },
                {
                  text: "Limit Points",
                  link: "/docs/elementary-topology/chapter-06/limit-points",
                },
                {
                  text: "Continuity and Convergence",
                  link: "/docs/elementary-topology/chapter-06/continuity-and-convergence",
                },
                {
                  text: "Filters",
                  link: "/docs/elementary-topology/chapter-06/filters",
                },
                {
                  text: "Ultranets and Ultrafilters",
                  link: "/docs/elementary-topology/chapter-06/ultranets-and-ultrafilters",
                },
              ],
            },
            {
              text: "7 · Covering Properties",
              link: "/docs/elementary-topology/chapter-07/",
              items: [
                {
                  text: "Open Covers and Refinements",
                  link: "/docs/elementary-topology/chapter-07/open-covers-and-refinements",
                },
                {
                  text: "Countability Properties",
                  link: "/docs/elementary-topology/chapter-07/countability-properties",
                },
                {
                  text: "Compactness",
                  link: "/docs/elementary-topology/chapter-07/compactness",
                },
                {
                  text: "Derived Spaces and Compactness",
                  link: "/docs/elementary-topology/chapter-07/derived-spaces-and-compactness",
                },
              ],
            },
            {
              text: "8 · More About Compactness",
              link: "/docs/elementary-topology/chapter-08/",
              items: [
                {
                  text: "Compactness in Euclidean Space",
                  link: "/docs/elementary-topology/chapter-08/compactness-in-euclidean-space",
                },
                {
                  text: "Local Compactness",
                  link: "/docs/elementary-topology/chapter-08/local-compactness",
                },
                {
                  text: "Compactifications",
                  link: "/docs/elementary-topology/chapter-08/compactifications",
                },
                {
                  text: "Sequential and Countable Compactness",
                  link: "/docs/elementary-topology/chapter-08/sequential-and-countable-compactness",
                },
              ],
            },
            {
              text: "9 · Connectedness",
              link: "/docs/elementary-topology/chapter-09/",
              items: [
                {
                  text: "The Notion of Connectedness",
                  link: "/docs/elementary-topology/chapter-09/notion-of-connectedness",
                },
                {
                  text: "Further Tests for Connectedness",
                  link: "/docs/elementary-topology/chapter-09/further-tests-for-connectedness",
                },
                {
                  text: "Connectedness and Derived Spaces",
                  link: "/docs/elementary-topology/chapter-09/connectedness-and-derived-spaces",
                },
                {
                  text: "Components. Local Connectedness",
                  link: "/docs/elementary-topology/chapter-09/components-and-local-connectedness",
                },
                {
                  text: "Connectedness and Compact T₂-Spaces",
                  link: "/docs/elementary-topology/chapter-09/connectedness-and-compact-t2-spaces",
                },
              ],
            },
            {
              text: "10 · Metrizability. Complete Metric Spaces",
              link: "/docs/elementary-topology/chapter-10/",
              items: [
                {
                  text: "Metrizable Spaces",
                  link: "/docs/elementary-topology/chapter-10/metrizable-spaces",
                },
                {
                  text: "Cauchy Sequences",
                  link: "/docs/elementary-topology/chapter-10/cauchy-sequences",
                },
                {
                  text: "Complete Metric Spaces",
                  link: "/docs/elementary-topology/chapter-10/complete-metric-spaces",
                },
                {
                  text: "Baire Category Theorem",
                  link: "/docs/elementary-topology/chapter-10/baire-category-theorem",
                },
                {
                  text: "Paracompactness. Complete Regularity",
                  link: "/docs/elementary-topology/chapter-10/paracompactness-and-complete-regularity",
                },
              ],
            },
            {
              text: "11 · Introduction to Homotopy Theory",
              link: "/docs/elementary-topology/chapter-11/",
              items: [
                {
                  text: "Homotopic Functions",
                  link: "/docs/elementary-topology/chapter-11/homotopic-functions",
                },
                {
                  text: "Loops",
                  link: "/docs/elementary-topology/chapter-11/loops",
                },
                {
                  text: "The Fundamental Group",
                  link: "/docs/elementary-topology/chapter-11/fundamental-group",
                },
                {
                  text: "The Fundamental Group and Continuous Functions",
                  link: "/docs/elementary-topology/chapter-11/fundamental-group-and-continuous-functions",
                },
              ],
            },
            {
              text: "Appendix and Indexes",
              link: "/docs/elementary-topology/back-matter/",
              items: [
                {
                  text: "Appendix on Infinite Products",
                  link: "/docs/elementary-topology/back-matter/appendix-on-infinite-products",
                },
                {
                  text: "Index of Symbols",
                  link: "/docs/elementary-topology/back-matter/index-of-symbols",
                },
                {
                  text: "Index",
                  link: "/docs/elementary-topology/back-matter/subject-index",
                },
              ],
            },
            { text: "Reusable TSX API", link: "/docs/elementary-topology/api" },
            {
              text: "Further Illustrations",
              link: "/docs/elementary-topology/further-illustrations",
            },
          ],
        },
      ],
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
      "/docs/bloom": [
        {
          text: "Bloom",
          items: [
            {
              text: "Tutorial",
              items: [
                {
                  text: "Getting Started",
                  link: "/docs/bloom/tutorial/getting_started",
                },
                {
                  text: "Hello, Diagram",
                  link: "/docs/bloom/tutorial/hello_diagram",
                },
                {
                  text: "Procedural Diagramming and Optimization",
                  link: "/docs/bloom/tutorial/optimization",
                },
                {
                  text: "Interactivity",
                  link: "/docs/bloom/tutorial/interactivity",
                },
              ],
            },
            {
              text: "Examples",
              link: "/docs/bloom/examples",
            },
            {
              text: "<a href='/bloom-docs/index.html' target='_blank'>Reference</a>",
            },
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
            {
              text: "Interactivity (experimental)",
              link: "/docs/ref/Interactivity",
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
          text: "September 2024",
          items: [
            {
              text: "Bloom: Optimization-Driven Interactive Diagramming",
              link: "/blog/bloom",
            },
          ],
        },
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
