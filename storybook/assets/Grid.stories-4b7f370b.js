var f=Object.defineProperty;var s=(t,e)=>f(t,"name",{value:e,configurable:!0});import{l as u}from"./svg-45390b6b.js";import{s as a,F as y}from"./styled-components.browser.esm-946f321d.js";import{i as x}from"./Simple-c9a21b09.js";import{R as C,r as v,j as o}from"./jsx-runtime-f76f309b.js";import{G,p as S}from"./penroseBlue-bdb40d77.js";import{c as b}from"./PenrosePrograms-7599ae95.js";import"./iframe-152c6bd8.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-d9b65618.js";import"./vector-wedge.substance-f3f6e6e8.js";const M=a.main`
  flex-grow: 1;
  margin-left: "4rem";
`,P=a.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,_=a.div``,V=a.h3`
  font-family: "Roboto Mono";
  color: ${t=>t.theme.primary};
`,d=class d extends C.Component{constructor(e){super(e),this.state={optimized:Array(e.diagrams.length)}}innerContent(){return this.props.diagrams.map(({substance:e,domain:c,style:g,variation:h},r)=>v.createElement(G,{...this.props.gridBoxProps,key:`grid-${r}`,name:`grid-${r}`,header:this.props.header(r),metadata:this.props.metadata(r),domain:c,style:g,gridIndex:r,substance:e,variation:h,excludeWarnings:[],onSelected:this.props.onSelected,onStateUpdate:(p,m)=>{this.setState(l=>{const i=[...l.optimized];return i[p]=x(m),this.props.onComplete&&u.every(i)&&this.props.onComplete(),{...l,optStatuses:i}}),this.props.onStateUpdate(p,m)},imageResolver:this.props.imageResolver,selected:this.props.selected&&this.props.selected.includes(r)}))}render(){const e=this.props.diagrams.length===0?o(_,{children:o(V,{children:"(Generated diagrams will appear here)"})}):this.innerContent();return o(M,{children:o(P,{children:e})})}};s(d,"Grid");let n=d;try{n.displayName="Grid",n.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},metadata:{defaultValue:null,description:"",name:"metadata",required:!0,type:{name:"(i: number) => { name: string; data: string; }[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},onSelected:{defaultValue:null,description:"",name:"onSelected",required:!1,type:{name:"((n: number) => void)"}},onComplete:{defaultValue:null,description:"",name:"onComplete",required:!1,type:{name:"(() => void)"}},onStateUpdate:{defaultValue:null,description:"",name:"onStateUpdate",required:!0,type:{name:"(n: number, s: State) => void"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},gridBoxProps:{defaultValue:null,description:"",name:"gridBoxProps",required:!1,type:{name:"Partial<GridboxProps>"}},selected:{defaultValue:null,description:"",name:"selected",required:!1,type:{name:"number[]"}}}}}catch{}const I={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:17},endLoc:{col:1,line:27},startBody:{col:46,line:17},endBody:{col:1,line:27}}}}},title:"Example/Grid Component",component:n},q=s(t=>o(y,{theme:S,children:o(n,{...t,header:e=>`Diagram ${e}`,metadata:e=>[{name:"Variation",data:t.diagrams[e].variation}]})}),"Template"),B=q.bind({});B.args={diagrams:u.range(10).map(t=>({...b,variation:`${t}`}))};const L=["ContinuousMap"];export{B as ContinuousMap,L as __namedExportsOrder,I as default};
//# sourceMappingURL=Grid.stories-4b7f370b.js.map
