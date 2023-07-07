var V=Object.defineProperty;var c=(t,l)=>V(t,"name",{value:l,configurable:!0});import r from"./vector-wedge.trio-f1eeaebc.js";import a from"./c05p13.trio-b4ea2ac1.js";import n from"./laplace-estimator.trio-dbb7634b.js";import{r as s,j as e,a as m,F as j}from"./jsx-runtime-671a6473.js";import{s as i}from"./styled-components.browser.esm-cd30b976.js";import{S as O}from"./Simple-9d2db06d.js";import{R as B,L as I}from"./Resample-8e352f9b.js";import"./vector-wedge.substance-9f00dd17.js";import"./resolver-2719b1fe.js";import"./iframe-4581b948.js";import"./euclidean.style-5c57c13b.js";import"./geometry.domain-952a3e66.js";import"./walk-on-spheres.domain-37de10c5.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-e9d7308d.js";import"./svg-ee647ab8.js";import"./CollectLabels-452d0f40.js";const T=i.div`
  position: relative;
  border-radius: 10px;
  border: 0.5px solid rgba(0, 0, 0, 0.2);
  box-shadow: 0 5px 8px 0 rgba(0, 0, 0, 0.2);
  background-color: #fff;
  overflow: hidden;
  min-height: 320px;
`,z=i.div`
  display: flex;
  justify-content: center;
  align-items: center;
  position: absolute;
  background-color: #0001;
  font-size: 20px;
  color: #000;
  left: 0;
  top: 0;
  right: 0;
  bottom: 0;
  cursor: pointer;
`,g=i.div`
  border-radius: 5px;
  background-color: ${t=>t.$active?"#40b4f7":"#bbb"};
  padding: 1px 3px;
  align-self: start;
  color: white;
`,E=i.div`
  display: flex;
  margin-right: auto;
  font-family: "Open Sans", sans-serif;
  font-size: 14px;
  width: 100%;
  flex-wrap: wrap;
  justify-content: center;
  margin: 10px 0px;
`,y=c(t=>{const{trio:l,imageResolver:u}=t,{variation:v,substance:b,style:f,domain:S}=l,[x,h]=s.useState(v),[k,D]=s.useState(0),[w,C]=s.useState(!1),[R,_]=s.useState([""]);return e(T,{children:w?m(j,{children:[e(O,{name:"embed",domain:S,substance:b,style:f,variation:x,interactive:!1,animate:!0,stepSize:5,imageResolver:u,onFrame:o=>{_(o.optStages),D(o.currentStageIndex)}}),m(E,{children:[R.map((o,L)=>{const p=o===""?"default":o;return L===k?e(g,{$active:!0,children:p}):e(g,{children:p})}),e("div",{onClick:()=>h(Math.random().toString()),style:{cursor:"pointer"},children:e(B,{size:28,color:"black"})})]})]}):m("div",{style:{display:"flex",alignItems:"center",justifyContent:"center"},children:[e(z,{onClick:()=>C(!0),children:"Click to lay out the diagram"}),e(I,{width:350,color:"#0001"})]})})},"StagedDiagram$1");try{StagedDiagram.displayName="StagedDiagram",StagedDiagram.__docgenInfo={description:"",displayName:"StagedDiagram",props:{trio:{defaultValue:null,description:"",name:"trio",required:!0,type:{name:"{ substance: string; domain: string; style: string; variation: string; }"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!0,type:{name:"PathResolver"}}}}}catch{}const ae={parameters:{storySource:{source:`import vector from "@penrose/examples/dist/exterior-algebra/vector-wedge.trio";
import geometry from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p13.trio";
import laplace from "@penrose/examples/dist/walk-on-spheres/laplace-estimator.trio.js";

import { ComponentMeta, ComponentStory } from "@storybook/react";
import StagedDiagram from "../StagedDiagram";

export default {
  title: "Example/StagedDiagram Component",
  component: StagedDiagram,
} as ComponentMeta<typeof StagedDiagram>;

const Template: ComponentStory<typeof StagedDiagram> = (args) => (
  <div style={{ width: "50%" }}>
    <StagedDiagram {...args} />
  </div>
);

export const WalkOnSphere = Template.bind({});
WalkOnSphere.args = {
  trio: {
    substance: laplace.substance,
    style: laplace.style[0].contents,
    domain: laplace.domain,
    variation: "test3",
  },
  imageResolver: laplace.style[0].resolver,
};
export const Geometry = Template.bind({});
Geometry.args = {
  trio: {
    substance: geometry.substance,
    style: geometry.style[0].contents,
    domain: geometry.domain,
    variation: "test",
  },
  imageResolver: geometry.style[0].resolver,
};
export const Vector = Template.bind({});
Vector.args = {
  trio: {
    substance: vector.substance,
    style: vector.style[0].contents,
    domain: vector.domain,
    variation: "test",
  },
  imageResolver: vector.style[0].resolver,
};
`,locationsMap:{"walk-on-sphere":{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}},geometry:{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}},vector:{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}}}}},title:"Example/StagedDiagram Component",component:y},d=c(t=>e("div",{style:{width:"50%"},children:e(y,{...t})}),"Template"),G=d.bind({});G.args={trio:{substance:n.substance,style:n.style[0].contents,domain:n.domain,variation:"test3"},imageResolver:n.style[0].resolver};const M=d.bind({});M.args={trio:{substance:a.substance,style:a.style[0].contents,domain:a.domain,variation:"test"},imageResolver:a.style[0].resolver};const W=d.bind({});W.args={trio:{substance:r.substance,style:r.style[0].contents,domain:r.domain,variation:"test"},imageResolver:r.style[0].resolver};const ne=["WalkOnSphere","Geometry","Vector"];export{M as Geometry,W as Vector,G as WalkOnSphere,ne as __namedExportsOrder,ae as default};
//# sourceMappingURL=StagedDiagram.stories-a9f005e7.js.map
