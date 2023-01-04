import{r as a}from"./index.5c77362b.js";import{L as p}from"./Listing.2ba7275b.js";import{S as u}from"./Simple.907861c9.js";import{a as g,j as n}from"./jsx-runtime.1c361ae2.js";import{v as r,a as o}from"./PenrosePrograms.0ec5f1d6.js";import"./index.33068e5f.js";const i=e=>{const[s,m]=a.exports.useState(0);a.exports.useEffect(()=>{const l=window.setInterval(()=>m(c=>(c+1)%e.examples.length),5e3);return()=>clearInterval(l)},[s]);const t=e.examples[s];return g("div",{style:{display:"flex",flexDirection:"row",height:"100%",flexWrap:"wrap"},children:[n(p,{domain:t.dsl,substance:t.sub,width:e.width,height:e.width,monacoOptions:{theme:e.darkMode?"vs-dark":"vs",wrappingIndent:"indent"}}),n("div",{style:{width:e.width,height:e.width},children:n(u,{substance:t.sub,style:t.sty,domain:t.dsl,variation:t.variation,interactive:!1,animate:!0})})]})};var d=i;i.__docgenInfo={description:"",methods:[],displayName:"Demo",props:{examples:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}`,signature:{properties:[{key:"sub",value:{name:"string",required:!0}},{key:"sty",value:{name:"string",required:!0}},{key:"dsl",value:{name:"string",required:!0}},{key:"variation",value:{name:"string",required:!0}}]}}],raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}[]`},description:""},width:{required:!0,tsType:{name:"string"},description:""},darkMode:{required:!0,tsType:{name:"boolean"},description:""}}};var k={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"vector-examples":{startLoc:{col:46,line:18},endLoc:{col:1,line:22},startBody:{col:46,line:18},endBody:{col:1,line:22}}}}},title:"Example/Demo Component",component:d};const v=e=>n("div",{style:{width:"100%",height:"100%"},children:n(d,{...e})}),y=v.bind({});y.args={examples:[{sub:r.substance,sty:r.style,dsl:r.domain,variation:r.variation},{sub:o.substance,sty:o.style,dsl:o.domain,variation:o.variation}],width:"400px"};const E=["VectorExamples"];export{y as VectorExamples,E as __namedExportsOrder,k as default};
//# sourceMappingURL=Demo.stories.9716976b.js.map
