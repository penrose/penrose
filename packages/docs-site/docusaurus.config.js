// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");

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
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Docs",
            items: [
              {
                label: "Reference",
                to: "/docs/ref",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Twitter",
                href: "https://twitter.com/UsePenrose",
              },
            ],
          },
          {
            title: "More",
            items: [
              {
                label: "GitHub",
                href: "https://github.com/penrose",
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
