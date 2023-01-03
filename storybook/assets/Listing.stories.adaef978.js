import{L as e}from"./Listing.de012a4a.js";import{h as n,o as i}from"./PenrosePrograms.ee62bd37.js";import{j as t}from"./jsx-runtime.1c361ae2.js";import"./index.5c77362b.js";import"./index.33068e5f.js";var g={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import Listing from "../Listing";
import { continuousMap, oneSet } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Listing Component",
  component: Listing,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Listing>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Listing> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Listing {...args} />
  </div>
);

export const ContinuousMapDark = Template.bind({});
ContinuousMapDark.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
  monacoOptions: {
    theme: "vs-dark",
  },
};

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
};

export const OneSet = Template.bind({});
OneSet.args = {
  domain: continuousMap.domain,
  substance: oneSet.substance,
  width: "400px",
  height: "300px",
};
`,locationsMap:{"continuous-map-dark":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}},"continuous-map":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}}}}},title:"Example/Listing Component",component:e};const o=s=>t("div",{style:{width:"100%",height:"100%"},children:t(e,{...s})}),a=o.bind({});a.args={domain:n.domain,substance:n.substance,width:"400px",height:"300px",monacoOptions:{theme:"vs-dark"}};const r=o.bind({});r.args={domain:n.domain,substance:n.substance,width:"400px",height:"300px"};const p=o.bind({});p.args={domain:n.domain,substance:i.substance,width:"400px",height:"300px"};const h=["ContinuousMapDark","ContinuousMap","OneSet"];export{r as ContinuousMap,a as ContinuousMapDark,p as OneSet,h as __namedExportsOrder,g as default};
//# sourceMappingURL=Listing.stories.adaef978.js.map
