var f=Object.defineProperty;var s=(t,e)=>f(t,"name",{value:e,configurable:!0});import{z as l}from"./index-12f33557.js";import{s as a,F as y}from"./styled-components.browser.esm-e68be8f2.js";import{R as x,r as C}from"./index-74c5fbfa.js";import{G as v,p as S}from"./penroseBlue-cfd8b227.js";import{j as o}from"./jsx-runtime-9c5bc5e6.js";import{c as G}from"./PenrosePrograms-54188900.js";import"./es.object.get-own-property-descriptor-2f3bcc00.js";import"./web.url-1059d872.js";import"./index-b79ff3a9.js";import"./Simple-905c8469.js";const P=a.main`
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
`;class n extends x.Component{constructor(e){super(e),this.state={optStatuses:Array(e.diagrams.length)}}innerContent(){return this.props.diagrams.map(({substance:e,domain:u,style:c,variation:g},r)=>C.createElement(v,{...this.props.gridBoxProps,key:`grid-${r}`,name:`grid-${r}`,header:this.props.header(r),metadata:this.props.metadata(r),domain:u,style:c,gridIndex:r,substance:e,variation:g,onSelected:this.props.onSelected,onStateUpdate:(d,p)=>{this.setState(m=>{const i=[...m.optStatuses];return i[d]=p.params.optStatus,this.props.onComplete&&l.every(i,h=>h==="EPConverged")&&this.props.onComplete(),{...m,optStatuses:i}}),this.props.onStateUpdate(d,p)},imageResolver:this.props.imageResolver,selected:this.props.selected&&this.props.selected.includes(r)}))}render(){const e=this.props.diagrams.length===0?o(M,{children:o(_,{children:"(Generated diagrams will appear here)"})}):this.innerContent();return o(P,{children:o(b,{children:e})})}}s(n,"Grid");try{n.displayName="Grid",n.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},metadata:{defaultValue:null,description:"",name:"metadata",required:!0,type:{name:"(i: number) => { name: string; data: string; }[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},onSelected:{defaultValue:null,description:"",name:"onSelected",required:!1,type:{name:"((n: number) => void)"}},onComplete:{defaultValue:null,description:"",name:"onComplete",required:!1,type:{name:"(() => void)"}},onStateUpdate:{defaultValue:null,description:"",name:"onStateUpdate",required:!0,type:{name:"(n: number, s: State) => void"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},gridBoxProps:{defaultValue:null,description:"",name:"gridBoxProps",required:!1,type:{name:"Partial<GridboxProps>"}},selected:{defaultValue:null,description:"",name:"selected",required:!1,type:{name:"number[]"}}}}}catch{}const I={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { range } from "lodash";
import { ThemeProvider } from "styled-components";
import { Grid } from "../Grid";
import penroseBlue from "../themes/penroseBlue";
import { continuousMap } from "./PenrosePrograms";

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
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:17},endLoc:{col:1,line:27},startBody:{col:46,line:17},endBody:{col:1,line:27}}}}},title:"Example/Grid Component",component:n},V=s(t=>o(y,{theme:S,children:o(n,{...t,header:e=>`Diagram ${e}`,metadata:e=>[{name:"Variation",data:t.diagrams[e].variation}]})}),"Template"),q=V.bind({});q.args={diagrams:l.range(10).map(t=>({...G,variation:`${t}`}))};const L=["ContinuousMap"];export{q as ContinuousMap,L as __namedExportsOrder,I as default};
//# sourceMappingURL=Grid.stories-e98c4a9e.js.map
