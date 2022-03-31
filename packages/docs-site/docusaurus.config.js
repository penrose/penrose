// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const math = require("remark-math");
const katex = require("rehype-katex");

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Penrose",
  tagline:
    "Create beautiful diagrams just by typing math notation in plain text.",
  url: "https://penrose.cs.cmu.edu",
  baseUrl: "/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/favicon.ico",
  organizationName: "penrose", // Usually your GitHub org/user name.
  projectName: "penrose", // Usually your repo name.
  plugins: ["./plugin-penrose-data"],
  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          remarkPlugins: [math],
          rehypePlugins: [katex],
          sidebarPath: require.resolve("./sidebars.js"),
          // Please change this to your repo.
          editUrl:
            "https://github.com/penrose/penrose/tree/main/packages/docs-site",
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],
  stylesheets: [
    {
      href: "https://cdn.jsdelivr.net/npm/katex@0.13.24/dist/katex.min.css",
      type: "text/css",
      integrity:
        "sha384-odtC+0UGzzFL/6PNoE8rX/SPcQDXBJ+uRepguP4QkPCm2LBxH3FA3y+fKSiJ+AmM",
      crossorigin: "anonymous",
    },
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: "Penrose",
        logo: {
          alt: "Penrose Logo",
          src: "img/logo.svg",
        },
        items: [
          {
            type: "doc",
            docId: "tutorial/welcome",
            position: "left",
            label: "Learn Penrose",
          },
          {
            type: "doc",
            docId: "ref/index",
            position: "left",
            label: "Documentation",
          },
          {
            href: "pathname:///try/",
            label: "Try Penrose",
            position: "left",
          },
          {
            type: "doc",
            docId: "team",
            position: "right",
            label: "Team",
          },
          {
            href: "https://github.com/penrose/penrose",
            label: "GitHub",
            position: "right",
          },
          {
            href: "https://twitter.com/UsePenrose",
            label: "Twitter",
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Using Penrose",
            items: [
              {
                label: "Learn Penrose",
                to: "/docs/tutorial/welcome",
              },
              {
                label: "Documentation",
                to: "/docs/ref",
              },
              {
                label: "Try Penrose",
                to: "pathname:///try/",
              },
            ],
          },
          {
            title: "Project",
            items: [
              {
                label: "GitHub",
                href: "https://github.com/penrose",
              },
              {
                label: "Twitter",
                href: "https://twitter.com/UsePenrose",
              },
            ],
          },
          {
            title: "News",
            items: [
              {
                label: "SIGGRAPH'20 Paper",
                to: "pathname:///siggraph20.html",
              },
              {
                label: "CHI'20 Paper",
                href:
                  "https://www.cs.cmu.edu/~woden/assets/chi-20-natural-diagramming.pdf",
              },
              {
                label: "Popular Mechanics",
                href:
                  "https://www.popularmechanics.com/science/math/a32743509/cmu-penrose-math-equations-into-pictures/",
              },
            ],
          },
        ],
        copyright: `made with ❤️ in Pittsburgh and abroad`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
