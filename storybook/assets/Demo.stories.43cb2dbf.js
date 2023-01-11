import{r as a}from"./index.5c77362b.js";import{S as l}from"./Simple.ed250e4e.js";import{j as o}from"./jsx-runtime.1c361ae2.js";import{v as n,a as r}from"./PenrosePrograms.dbb062c8.js";const i=e=>{const[s,d]=a.exports.useState(0);a.exports.useEffect(()=>{const c=window.setInterval(()=>d(p=>(p+1)%e.examples.length),5e3);return()=>clearInterval(c)},[s]);const t=e.examples[s];return o("div",{style:{width:e.width,height:e.width},children:o(l,{substance:t.sub,style:t.sty,domain:t.dsl,variation:t.variation,interactive:!1,animate:!0})})};var m=i;i.__docgenInfo={description:"",methods:[],displayName:"Demo",props:{examples:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}`,signature:{properties:[{key:"sub",value:{name:"string",required:!0}},{key:"sty",value:{name:"string",required:!0}},{key:"dsl",value:{name:"string",required:!0}},{key:"variation",value:{name:"string",required:!0}}]}}],raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}[]`},description:""},width:{required:!0,tsType:{name:"string"},description:""},darkMode:{required:!0,tsType:{name:"boolean"},description:""}}};var h={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import Demo from "../Demo";
import { vectorsPerp, vectorWedge } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Demo Component",
  component: Demo,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Demo>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Demo> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Demo {...args} />
  </div>
);

export const VectorExamples = Template.bind({});
VectorExamples.args = {
  examples: [
    {
      sub: vectorsPerp.substance,
      sty: vectorsPerp.style,
      dsl: vectorsPerp.domain,
      variation: vectorsPerp.variation,
    },
    {
      sub: vectorWedge.substance,
      sty: vectorWedge.style,
      dsl: vectorWedge.domain,
      variation: vectorWedge.variation,
    },
  ],
  width: "400px",
};
`,locationsMap:{"vector-examples":{startLoc:{col:46,line:18},endLoc:{col:1,line:22},startBody:{col:46,line:18},endBody:{col:1,line:22}}}}},title:"Example/Demo Component",component:m};const u=e=>o("div",{style:{width:"100%",height:"100%"},children:o(m,{...e})}),g=u.bind({});g.args={examples:[{sub:n.substance,sty:n.style,dsl:n.domain,variation:n.variation},{sub:r.substance,sty:r.style,dsl:r.domain,variation:r.variation}],width:"400px"};const f=["VectorExamples"];export{g as VectorExamples,f as __namedExportsOrder,h as default};
//# sourceMappingURL=Demo.stories.43cb2dbf.js.map
