var f=Object.defineProperty;var s=(t,e)=>f(t,"name",{value:e,configurable:!0});import{l}from"./svg-e2a9632f.js";import{s as a,F as y}from"./styled-components.browser.esm-c59d061c.js";import{R as x,r as C,j as o}from"./jsx-runtime-74202484.js";import{G as v,p as S}from"./penroseBlue-4b0a894a.js";import{c as G}from"./PenrosePrograms-1f2fa0da.js";import"./iframe-916e8a48.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-c205d1da.js";import"./Simple-2c01fb3e.js";import"./CollectLabels-35011149.js";import"./vector-wedge.substance-ed38d9fe.js";import"./resolver-8e6b3de6.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-21a98008.js";const P=a.main`
  flex-grow: 1;
  margin-left: "4rem";
`,b=a.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,M=a.div``,_=a.h3`
  font-family: "Roboto Mono";
  color: ${t=>t.theme.primary};
`;class n extends x.Component{constructor(e){super(e),this.state={optStatuses:Array(e.diagrams.length)}}innerContent(){return this.props.diagrams.map(({substance:e,domain:u,style:c,variation:g},r)=>C.createElement(v,{...this.props.gridBoxProps,key:`grid-${r}`,name:`grid-${r}`,header:this.props.header(r),metadata:this.props.metadata(r),domain:u,style:c,gridIndex:r,substance:e,variation:g,onSelected:this.props.onSelected,onStateUpdate:(d,p)=>{this.setState(m=>{const i=[...m.optStatuses];return i[d]=p.params.optStatus,this.props.onComplete&&l.every(i,h=>h==="EPConverged")&&this.props.onComplete(),{...m,optStatuses:i}}),this.props.onStateUpdate(d,p)},imageResolver:this.props.imageResolver,selected:this.props.selected&&this.props.selected.includes(r)}))}render(){const e=this.props.diagrams.length===0?o(M,{children:o(_,{children:"(Generated diagrams will appear here)"})}):this.innerContent();return o(P,{children:o(b,{children:e})})}}s(n,"Grid");try{n.displayName="Grid",n.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},metadata:{defaultValue:null,description:"",name:"metadata",required:!0,type:{name:"(i: number) => { name: string; data: string; }[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},onSelected:{defaultValue:null,description:"",name:"onSelected",required:!1,type:{name:"((n: number) => void)"}},onComplete:{defaultValue:null,description:"",name:"onComplete",required:!1,type:{name:"(() => void)"}},onStateUpdate:{defaultValue:null,description:"",name:"onStateUpdate",required:!0,type:{name:"(n: number, s: State) => void"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},gridBoxProps:{defaultValue:null,description:"",name:"gridBoxProps",required:!1,type:{name:"Partial<GridboxProps>"}},selected:{defaultValue:null,description:"",name:"selected",required:!1,type:{name:"number[]"}}}}}catch{}const O={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { range } from "lodash";
import { ThemeProvider } from "styled-components";
import { Grid } from "../Grid.js";
import penroseBlue from "../themes/penroseBlue.js";
import { continuousMap } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Grid Component",
  component: Grid,
} as ComponentMeta<typeof Grid>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Grid> = (args) => (
  <ThemeProvider theme={penroseBlue}>
    <Grid
      {...args}
      header={(i) => \`Diagram \${i}\`}
      metadata={(i) => [
        { name: "Variation", data: args.diagrams[i].variation },
      ]}
    />
  </ThemeProvider>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  diagrams: range(10).map((n) => ({
    ...continuousMap,
    variation: \`\${n}\`,
  })),
};
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:17},endLoc:{col:1,line:27},startBody:{col:46,line:17},endBody:{col:1,line:27}}}}},title:"Example/Grid Component",component:n},V=s(t=>o(y,{theme:S,children:o(n,{...t,header:e=>`Diagram ${e}`,metadata:e=>[{name:"Variation",data:t.diagrams[e].variation}]})}),"Template"),q=V.bind({});q.args={diagrams:l.range(10).map(t=>({...G,variation:`${t}`}))};const z=["ContinuousMap"];export{q as ContinuousMap,z as __namedExportsOrder,O as default};
//# sourceMappingURL=Grid.stories-fe1ba0b5.js.map
