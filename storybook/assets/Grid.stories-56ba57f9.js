var h=Object.defineProperty;var s=(t,e)=>h(t,"name",{value:e,configurable:!0});import{l}from"./svg-ca0c608d.js";import{s as a,F as f}from"./styled-components.browser.esm-3d2f5315.js";import{i as y}from"./Simple-e159516c.js";import{R as x,r as C,j as o}from"./jsx-runtime-433938a6.js";import{G as v,p as G}from"./penroseBlue-abd2e919.js";import{c as S}from"./PenrosePrograms-cd468070.js";import"./iframe-f6c56226.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-f51ff477.js";import"./vector-wedge.substance-c51fe7ca.js";const b=a.main`
  flex-grow: 1;
  margin-left: "4rem";
`,M=a.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,P=a.div``,_=a.h3`
  font-family: "Roboto Mono";
  color: ${t=>t.theme.primary};
`;class n extends x.Component{constructor(e){super(e),this.state={optimized:Array(e.diagrams.length)}}innerContent(){return this.props.diagrams.map(({substance:e,domain:u,style:c,variation:g},r)=>C.createElement(v,{...this.props.gridBoxProps,key:`grid-${r}`,name:`grid-${r}`,header:this.props.header(r),metadata:this.props.metadata(r),domain:u,style:c,gridIndex:r,substance:e,variation:g,excludeWarnings:[],onSelected:this.props.onSelected,onStateUpdate:(d,p)=>{this.setState(m=>{const i=[...m.optimized];return i[d]=y(p),this.props.onComplete&&l.every(i)&&this.props.onComplete(),{...m,optStatuses:i}}),this.props.onStateUpdate(d,p)},imageResolver:this.props.imageResolver,selected:this.props.selected&&this.props.selected.includes(r)}))}render(){const e=this.props.diagrams.length===0?o(P,{children:o(_,{children:"(Generated diagrams will appear here)"})}):this.innerContent();return o(b,{children:o(M,{children:e})})}}s(n,"Grid");try{n.displayName="Grid",n.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},metadata:{defaultValue:null,description:"",name:"metadata",required:!0,type:{name:"(i: number) => { name: string; data: string; }[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},onSelected:{defaultValue:null,description:"",name:"onSelected",required:!1,type:{name:"((n: number) => void)"}},onComplete:{defaultValue:null,description:"",name:"onComplete",required:!1,type:{name:"(() => void)"}},onStateUpdate:{defaultValue:null,description:"",name:"onStateUpdate",required:!0,type:{name:"(n: number, s: State) => void"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},gridBoxProps:{defaultValue:null,description:"",name:"gridBoxProps",required:!1,type:{name:"Partial<GridboxProps>"}},selected:{defaultValue:null,description:"",name:"selected",required:!1,type:{name:"number[]"}}}}}catch{}const F={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:17},endLoc:{col:1,line:27},startBody:{col:46,line:17},endBody:{col:1,line:27}}}}},title:"Example/Grid Component",component:n},V=s(t=>o(f,{theme:G,children:o(n,{...t,header:e=>`Diagram ${e}`,metadata:e=>[{name:"Variation",data:t.diagrams[e].variation}]})}),"Template"),q=V.bind({});q.args={diagrams:l.range(10).map(t=>({...S,variation:`${t}`}))};const I=["ContinuousMap"];export{q as ContinuousMap,I as __namedExportsOrder,F as default};
//# sourceMappingURL=Grid.stories-56ba57f9.js.map
