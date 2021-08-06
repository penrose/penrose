import React from "react";
import {
  compileDomain,
  compileSubstance,
  showError,
  SubProg,
  Synthesizer,
  SynthesizerSetting,
} from "@penrose/core";
import { Grid } from "./Grid";
import { defaultSetting, Settings } from "./Settings";
import styled from "styled-components";
import { DownloadSVG } from "../utils/utils";

export type ContentProps = any;

export interface SynthesizedSubstance {
  prog: SubProg;
  ops: string;
}

export interface ContentState {
  setting: SynthesizerSetting;
  progs: SynthesizedSubstance[];
  staged: [number, string][];
}

const ContentSection = styled.section`
  display: flex;
  flex-direction: row;
  width: 100%;
  height: auto;
  overflow: hidden;
`;

const Header = styled.section`
  display: flex;
  flex-direction: row;
  width: calc(100vw-2rem);
  height: 4rem;
  justify-content: space-between;
  padding-left: 1rem;
  padding-right: 1rem;
  border-bottom: 1px solid gray;
`;

const H1 = styled.h1``;

const Btn = styled.button`
  display: inline-block;
  color: gray;
  font-size: 1rem;
  margin: 1rem;
  width: 4.5rem;
  height: 2rem;
  padding: 0.25rem 1rem;
  border: 2px solid gray;
  border-radius: 0.25rem;
  display: flex;
  justify-content: center;
  align-items: center;
`;

const ExportBtn = styled(Btn)`
  background-color: purple;
  border: none;
  color: white;
`;

export class Content extends React.Component<ContentProps, ContentState> {
  private domain: string;
  private style: string;
  constructor(props: ContentProps) {
    super(props);
    this.state = {
      setting: defaultSetting,
      progs: [],
      staged: [],
    };
    this.domain = "";
    this.style = "";
  }

  componentDidMount() {
    fetch("public/files/geometry.txt")
      .then((r) => r.text())
      .then((text) => {
        this.domain = text;
      });
    fetch("public/files/euclidean.txt")
      .then((r) => r.text())
      .then((text) => {
        this.style = text;
      });
  }

  updateSettings = () => (newSetting: SynthesizerSetting) => {
    this.setState({
      setting: newSetting,
    });
  };

  addStaged = (idx: number, svgStr: string) => {
    if (svgStr !== "") {
      const newStaged = this.state.staged;
      const index = newStaged.map((item) => item[0]).indexOf(idx);
      if (index > -1) {
        // delete object from array if it was already staged (i.e. checkbox was unchecked)
        newStaged.splice(index, 1);
      } else {
        newStaged.push([idx, svgStr]);
        this.setState({ staged: newStaged });
      }
    }
  };

  generateProgs = () => (prompt: string) => {
    console.log("prompt", prompt);
    const envOrError = compileDomain(this.domain);

    // initialize synthesizer
    if (envOrError.isOk()) {
      const env = envOrError.value;
      let subResult;
      if (prompt.length > 0) {
        const subRes = compileSubstance(prompt, env);
        if (subRes.isOk()) {
          subResult = subRes.value;
        } else {
          console.log(
            `Error when compiling the template Substance program: ${showError(
              subRes.error
            )}`
          );
        }
      }
      const synth = new Synthesizer(env, this.state.setting, subResult);
      let progs = synth.generateSubstances(this.state.setting.numPrograms);
      const template: SubProg | undefined = synth.getTemplate();
      if (template) {
        this.setState({ progs: [{ prog: template }, ...progs], staged: [] });
      }
    }
  };

  exportDiagrams = () => {
    this.state.staged.map(([idx, svg]) => {
      DownloadSVG(svg, `diagram_${idx}`);
    });
  };

  render() {
    console.log("re-rendering content", this.state.progs.length);
    return (
      <div>
        <Header>
          <H1>Edgeworth</H1>
          <ExportBtn onClick={this.exportDiagrams}>Export</ExportBtn>
        </Header>
        <ContentSection>
          <Settings
            updateSettings={this.updateSettings}
            generateCallback={this.generateProgs()}
          />
          <Grid
            style={this.style}
            domain={this.domain}
            progs={this.state.progs}
            onStaged={this.addStaged}
          />
        </ContentSection>
      </div>
    );
  }
}
