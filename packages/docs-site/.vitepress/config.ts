import { defineConfig } from "vitepress";

export default defineConfig({
  ignoreDeadLinks: true,
  outDir: "build",
  title: "Penrose",
  description:
    "Create beautiful diagrams just by typing math notation in plain text.",
  head: [["link", { rel: "icon", href: "/img/logo.svg" }]],
  cleanUrls: "with-subfolders",
  themeConfig: {
    logo: "img/favicon.ico",
    nav: [
      {
        text: "Learn Penrose",
        link: "/docs/tutorial/welcome",
        activeMatch: "/docs/tutorial",
      },
      { text: "Documentation", link: "/docs/ref", activeMatch: "/docs/ref" },
      { text: "Try Penrose", link: "/try/index.html" },
      { text: "Team", link: "/docs/team" },
      {
        text: "News",
        items: [
          { text: "SIGGRAPH'20 paper", link: "/siggraph20.html" },
          {
            text: "CHI'20 paper",
            link:
              "https://www.cs.cmu.edu/~woden/assets/chi-20-natural-diagramming.pdf",
          },
          {
            text: "Popular Mechanics",
            link:
              "https://www.popularmechanics.com/science/math/a32743509/cmu-penrose-math-equations-into-pictures/",
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
            {
              text: "Writing Constraints & Objectives",
              link: "/docs/tutorial/constraints",
            },
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
                { text: "Syntax", link: "/docs/ref/domain/syntax" },
                { text: "Examples", link: "/docs/ref/domain/examples" },
              ],
            },
            {
              text: "Substance",
              items: [
                { text: "Overview", link: "/docs/ref/substance" },
                { text: "Syntax", link: "/docs/ref/substance/syntax" },
                { text: "Examples", link: "/docs/ref/substance/examples" },
              ],
            },
            {
              text: "Style",
              items: [
                { text: "Overview", link: "/docs/ref/style" },
                {
                  text: "Shape Library",
                  items: [
                    { text: "Circle", link: "/docs/ref/style/shapes/circle" },
                    { text: "Ellipse", link: "/docs/ref/style/shapes/ellipse" },
                    {
                      text: "Equation",
                      link: "/docs/ref/style/shapes/equation",
                    },
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
                { text: "Function Library", link: "/docs/ref/style/functions" },
                {
                  text: "Passthrough SVG",
                  link: "/docs/ref/style/passthrough",
                },
                {
                  text: "Syntax and Semantics",
                  link: "/docs/ref/style/syntax",
                },
                { text: "Examples", link: "/docs/ref/style/examples" },
              ],
            },
          ],
        },
      ],
    },
    footer: { copyright: "made with ❤️ in Pittsburgh and abroad" },
  },
});
