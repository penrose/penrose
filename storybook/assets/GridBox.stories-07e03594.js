var s=Object.defineProperty;var e=(o,a)=>s(o,"name",{value:a,configurable:!0});import{F as i}from"./styled-components.browser.esm-d8358605.js";import{G as r,p}from"./penroseBlue-836ed5ee.js";import{c as m,o as c,e as d}from"./PenrosePrograms-fcb11dfd.js";import{j as n}from"./jsx-runtime-c6652fc8.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-cf17fd1b.js";import"./iframe-fa2344d7.js";import"./Simple-f5c4006a.js";import"./svg-4a436c2c.js";import"./vector-wedge.substance-8426bbe1.js";import"./resolver-6b1c30e6.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-a0e1415b.js";const P={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},"one-set":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},error:{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}}}}},title:"Example/GridBox Component",component:r},t=e(o=>n("div",{style:{width:"50%",height:"50%"},children:n(i,{theme:p,children:n(r,{...o,metadata:[{name:"Variation",data:o.variation},{name:"Substance Program",data:o.substance}]})})}),"Template"),l=t.bind({});l.args=m;const u=t.bind({});u.args=c;const g=t.bind({});g.args=d;const L=["ContinuousMap","OneSet","Error"];export{l as ContinuousMap,g as Error,u as OneSet,L as __namedExportsOrder,P as default};
//# sourceMappingURL=GridBox.stories-07e03594.js.map
