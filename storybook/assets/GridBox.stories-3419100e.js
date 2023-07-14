var i=Object.defineProperty;var r=(o,a)=>i(o,"name",{value:a,configurable:!0});import{F as s}from"./styled-components.browser.esm-f64f0466.js";import{G as e,p}from"./penroseBlue-d7523f63.js";import{c as m,o as c,e as d}from"./PenrosePrograms-489e9980.js";import{j as t}from"./jsx-runtime-59972bed.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-3822f25d.js";import"./iframe-f5449c82.js";import"./Simple-faf73363.js";import"./svg-142e91b5.js";import"./CollectLabels-d14adb93.js";import"./vector-wedge.substance-93c89ac3.js";import"./resolver-11ab920e.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-9284244f.js";import"./setTheory.domain-681bfd26.js";import"./venn.style-946a708b.js";const w={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
//# sourceMappingURL=GridBox.stories-3419100e.js.map
