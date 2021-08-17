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
  domain: string;
  style: string;
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
  justifyContent: "space-between",
  background:
    "linear-gradient(162deg, rgba(63,81,181,1) 33%, rgba(10,21,83,1) 100%)",
});

const ButtonBox = styled(Box)({
  display: "flex",
  flexDirection: "row",
  flexWrap: "nowrap",
  padding: "0",
  alignItems: "center",
  justifyContent: "space-between",
});

const Title = styled(Typography)({
  fontFamily: "Roboto Mono",
  color: "white",
});

const StagedText = styled(Typography)(({ theme }) => ({
  color: theme.palette.info.light,
  fontSize: "0.8rem",
  padding: "0 1rem",
}));

const ExportButton = styled(Button)({
  // border: "2px solid white",
});

export class Content extends React.Component<ContentProps, ContentState> {
  constructor(props: ContentProps) {
    super(props);
    this.state = {
      progs: [],
      staged: [],
      domain: "",
      style: "",
    };
  }

  componentDidMount() {
    fetch("public/files/geometry.txt")
      .then((r) => r.text())
      .then((text) => {
        this.setState({ domain: text });
      });
    fetch("public/files/euclidean.txt")
      .then((r) => r.text())
      .then((text) => {
        this.setState({ style: text });
      });
  }

  // callback function to indicate that a svg will be exported
  addStaged = (idx: number, svgStr: string) => {
    if (svgStr !== "") {
      const newStaged = this.state.staged;
      const index = newStaged.map((item) => item[0]).indexOf(idx);
      if (index > -1) {
        // delete object from array if it was already staged (i.e. checkbox was unchecked)
        newStaged.splice(index, 1);
      } else {
        newStaged.push([idx, svgStr]);
      }
      this.setState({ staged: newStaged });
    }
  };

  generateProgs = () => (
    setting: SynthesizerSetting,
    numPrograms: number,
    dsl: string,
    prompt: string,
    sty: string
  ) => {
    const envOrError = compileDomain(dsl);

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
          domain: dsl,
          style: sty,
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
    return (
      <div>
        <AppBar position="fixed">
          <HeaderContent>
            <Title variant="h6" noWrap>
              EDGEWORTH
            </Title>
            <ButtonBox>
              <StagedText>{`${this.state.staged.length} diagrams selected`}</StagedText>
              <ExportButton
                variant="outlined"
                color="inherit"
                onClick={this.exportDiagrams}
              >
                Export
              </ExportButton>
            </ButtonBox>
          </HeaderContent>
        </AppBar>
        {/* NOTE: the Toolbar is used exclusively to space the content underneath the header of the page */}
        <Toolbar />
        <ContentSection>
          <Settings
            generateCallback={this.generateProgs()}
            defaultDomain={this.state.domain}
            defaultStyle={this.state.style}
          />
          <Grid
            style={this.state.style}
            domain={this.state.domain}
            progs={this.state.progs}
            onStaged={this.addStaged}
          />
        </ContentSection>
      </div>
    );
  }
}
