var i=Object.defineProperty;var e=(o,a)=>i(o,"name",{value:a,configurable:!0});import{F as s}from"./styled-components.browser.esm-7b3131aa.js";import{G as r,p}from"./penroseBlue-9695d776.js";import{c as m,o as c,e as d}from"./PenrosePrograms-f8d0aa71.js";import{j as n}from"./jsx-runtime-1414e3d9.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-30cd6556.js";import"./iframe-e4576df4.js";import"./Simple-d65ba2f9.js";import"./svg-4b9035ef.js";import"./CollectLabels-97fd8b95.js";import"./vector-wedge.substance-947a5291.js";import"./resolver-8467d754.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-cc9d3243.js";const L={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},"one-set":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},error:{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}}}}},title:"Example/GridBox Component",component:r},t=e(o=>n("div",{style:{width:"50%",height:"50%"},children:n(s,{theme:p,children:n(r,{...o,metadata:[{name:"Variation",data:o.variation},{name:"Substance Program",data:o.substance}]})})}),"Template"),l=t.bind({});l.args=m;const u=t.bind({});u.args=c;const g=t.bind({});g.args=d;const k=["ContinuousMap","OneSet","Error"];export{l as ContinuousMap,g as Error,u as OneSet,k as __namedExportsOrder,L as default};
//# sourceMappingURL=GridBox.stories-6190f673.js.map
