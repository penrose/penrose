import { Model } from "flexlayout-react";

export const topLayoutModel = Model.fromJson({
  global: {
    tabEnableFloat: true,
    tabSetMinWidth: 100,
    tabSetMinHeight: 100,
    borderMinSize: 100,
  },
  layout: {
    type: "row",
    children: [
      {
        type: "row",
        weight: 50,
        children: [
          {
            type: "tabset",
            weight: 80,
            children: [
              {
                type: "tab",
                name: "Style",
                id: "styleEditor",
                enableClose: false,
                enableRename: false,
              },
            ],
          },
          {
            type: "row",
            weight: 50,
            children: [
              {
                type: "tabset",
                weight: 50,
                children: [
                  {
                    type: "tab",
                    name: "Domain",
                    id: "domainProgramEditor",
                    enableClose: false,
                    enableRename: false,
                  },
                ],
              },
              {
                type: "tabset",
                weight: 50,
                children: [
                  {
                    type: "tab",
                    name: "Substance",
                    id: "substanceProgramEditor",
                    enableClose: false,
                    enableRename: false,
                  },
                ],
              },
            ],
          },
        ],
      },
      {
        type: "tabset",
        weight: 50,
        children: [
          {
            type: "tab",
            name: "Visualization",
            id: "vizPanel",
            enableClose: false,
            enableRename: false,
          },
        ],
      },
    ],
  },
});

export const styleEditorLayoutModel = Model.fromJson({
  global: {
    tabSetHeaderHeight: 0,
  },
  layout: {
    type: "row",
    children: [
      {
        type: "tabset",
        headerHeight: 0,
        enableTabStrip: false,
        children: [
          {
            type: "tab",
            name: "Style",
            id: "styleProgramEditor",
            enableClose: false,
          },
        ],
      },
    ],
  },
  borders: [
    {
      type: "border",
      location: "right",
      children: [
        {
          type: "tab",
          name: "Style Resources",
          id: "styleResourcesEditor",
          enableClose: false,
        },
      ],
    },
  ],
});
