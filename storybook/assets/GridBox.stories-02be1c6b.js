var i=Object.defineProperty;var r=(o,a)=>i(o,"name",{value:a,configurable:!0});import{F as s}from"./styled-components.browser.esm-272ab4a6.js";import{G as e,p}from"./penroseBlue-9cee357c.js";import{c as m,o as c,e as d}from"./PenrosePrograms-31e6ae0d.js";import{j as t}from"./jsx-runtime-50ff9bfa.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-c83fbcd2.js";import"./iframe-8d1c39a4.js";import"./Simple-f86ba08a.js";import"./svg-6cd73a06.js";import"./CollectLabels-be2bb010.js";import"./vector-wedge.substance-0e092761.js";import"./resolver-b9429209.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-8995dd1a.js";import"./setTheory.domain-681bfd26.js";import"./venn.style-bb2a7220.js";const w={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { ThemeProvider } from "styled-components";
import { Gridbox } from "../Gridbox.js";
import penroseBlue from "../themes/penroseBlue.js";
import { continuousMap, error, oneSet } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/GridBox Component",
  component: Gridbox,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Gridbox>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Gridbox> = (args) => (
  <div style={{ width: "50%", height: "50%" }}>
    <ThemeProvider theme={penroseBlue}>
      <Gridbox
        {...args}
        metadata={[
          { name: "Variation", data: args.variation },
          { name: "Substance Program", data: args.substance },
        ]}
      />
    </ThemeProvider>
  </div>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = continuousMap;

export const OneSet = Template.bind({});
OneSet.args = oneSet;

export const Error = Template.bind({});
Error.args = error;
`,locationsMap:{"continuous-map":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},"one-set":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},error:{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}}}}},title:"Example/GridBox Component",component:e},n=r(o=>t("div",{style:{width:"50%",height:"50%"},children:t(s,{theme:p,children:t(e,{...o,metadata:[{name:"Variation",data:o.variation},{name:"Substance Program",data:o.substance}]})})}),"Template"),l=n.bind({});l.args=m;const u=n.bind({});u.args=c;const g=n.bind({});g.args=d;const O=["ContinuousMap","OneSet","Error"];export{l as ContinuousMap,g as Error,u as OneSet,O as __namedExportsOrder,w as default};
//# sourceMappingURL=GridBox.stories-02be1c6b.js.map
