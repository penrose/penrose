var i=Object.defineProperty;var e=(o,a)=>i(o,"name",{value:a,configurable:!0});import{F as s}from"./styled-components.browser.esm-a8e0bf38.js";import{G as r,p}from"./penroseBlue-3dc4c77b.js";import{c as m,o as c,e as d}from"./PenrosePrograms-119f98e5.js";import{j as n}from"./jsx-runtime-e55abde1.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-b442a447.js";import"./iframe-1df500b0.js";import"./Simple-dbb2360d.js";import"./svg-3a0543f7.js";import"./CollectLabels-f6e9c32f.js";import"./vector-wedge.substance-b0d61792.js";import"./resolver-66ef16e8.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-a8fb5390.js";const L={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
//# sourceMappingURL=GridBox.stories-3e6dca85.js.map
