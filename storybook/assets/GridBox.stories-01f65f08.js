var s=Object.defineProperty;var e=(o,a)=>s(o,"name",{value:a,configurable:!0});import{F as i}from"./styled-components.browser.esm-e68be8f2.js";import{G as r,p}from"./penroseBlue-4474e4d5.js";import{c as m,o as c,e as d}from"./PenrosePrograms-28ddb5f1.js";import{j as n}from"./jsx-runtime-9c5bc5e6.js";import"./index-b79ff3a9.js";import"./index-74c5fbfa.js";import"./es.object.get-own-property-descriptor-2f3bcc00.js";import"./Simple-00d6b7a3.js";import"./index-e85d8290.js";import"./web.url-1059d872.js";const v={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { ThemeProvider } from "styled-components";
import { Gridbox } from "../Gridbox";
import penroseBlue from "../themes/penroseBlue";
import { continuousMap, error, oneSet } from "./PenrosePrograms";

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
`,locationsMap:{"continuous-map":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},"one-set":{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}},error:{startLoc:{col:49,line:20},endLoc:{col:1,line:32},startBody:{col:49,line:20},endBody:{col:1,line:32}}}}},title:"Example/GridBox Component",component:r},t=e(o=>n("div",{style:{width:"50%",height:"50%"},children:n(i,{theme:p,children:n(r,{...o,metadata:[{name:"Variation",data:o.variation},{name:"Substance Program",data:o.substance}]})})}),"Template"),l=t.bind({});l.args=m;const u=t.bind({});u.args=c;const g=t.bind({});g.args=d;const E=["ContinuousMap","OneSet","Error"];export{l as ContinuousMap,g as Error,u as OneSet,E as __namedExportsOrder,v as default};
//# sourceMappingURL=GridBox.stories-01f65f08.js.map
