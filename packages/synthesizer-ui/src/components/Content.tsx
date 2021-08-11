import React from "react";
import {
  compileDomain,
  compileSubstance,
  showError,
  SubProg,
  Synthesizer,
  SynthesizerSetting,
  SynthesizedSubstance,
} from "@penrose/core";
import { Grid } from "./Grid";
import { Settings } from "./Settings";
import { DownloadSVG } from "../utils/utils";
import { Button, Box, styled, Typography, Toolbar } from "@material-ui/core";
import { AppBar } from "@material-ui/core";

export type ContentProps = any;

export interface ContentState {
  progs: SynthesizedSubstance[];
  staged: [number, string][];
}

const ContentSection = styled(Box)({
  fontFamily: "Helvetica",
  display: "flex",
  flexDirection: "row",
  width: "100%",
  height: "auto",
  overflow: "hidden",
  margin: "0",
  padding: "0",
});

const HeaderContent = styled(Toolbar)({
  fontWeight: "bold",
  justifyContent: "space-between",
  background:
    "linear-gradient(162deg, rgba(63,81,181,1) 33%, rgba(10,21,83,1) 100%)",
});

const Title = styled(Typography)({
  fontFamily: "Roboto Mono",
  fontWeight: "lighter",
  color: "white",
});
export class Content extends React.Component<ContentProps, ContentState> {
  private domain: string;
  private style: string;
  constructor(props: ContentProps) {
    super(props);
    this.state = {
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

  generateProgs = () => (
    setting: SynthesizerSetting,
    numPrograms: number,
    prompt: string
  ) => {
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
      const synth = new Synthesizer(env, setting, subResult);
      let progs = synth.generateSubstances(numPrograms);
      const template: SubProg | undefined = synth.getTemplate();
      if (template) {
        this.setState({
          progs: [{ prog: template, ops: [] }, ...progs],
          staged: [],
        });
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
        <AppBar position="fixed">
          <HeaderContent>
            <Title variant="h6" noWrap>
              EDGEWORTH
            </Title>
            <Button color="inherit" onClick={this.exportDiagrams}>
              Export
            </Button>
          </HeaderContent>
        </AppBar>
        {/* NOTE: the Toolbar is used exclusively to space the content underneath the header of the page */}
        <Toolbar />
        <ContentSection>
          <Settings generateCallback={this.generateProgs()} />
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
