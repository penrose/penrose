import {
  AppBar,
  Box,
  Button,
  styled,
  Toolbar,
  Typography,
} from "@material-ui/core";
import {
  compileDomain,
  compileSubstance,
  PenroseState,
  prettySubstance,
  RenderStatic,
  showError,
  showMutations,
  SubProg,
  SynthesizedSubstance,
  Synthesizer,
  SynthesizerSetting,
} from "@penrose/core";
import { A } from "@penrose/core/build/dist/types/ast";
import { saveAs } from "file-saver";
import JSZip from "jszip";
import { range } from "lodash";
import React from "react";
import { Grid } from "./Grid";
import { Settings } from "./Settings";

export type ContentProps = any;

export interface ContentState {
  progs: SynthesizedSubstance[];
  states: PenroseState[];
  staged: number[];
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
  color: "white",
  fontSize: "0.8rem",
  padding: "0 1rem",
}));

export class Content extends React.Component<ContentProps, ContentState> {
  constructor(props: ContentProps) {
    super(props);
    this.state = {
      progs: [],
      states: [],
      staged: [],
      domain: "",
      style: "",
    };
  }
  // callback function to indicate that a svg will be exported
  addStaged = (idx: number) => {
    let newStaged = [...this.state.staged];
    if (this.state.staged.includes(idx)) {
      // delete object from array if it was already staged (i.e. checkbox was unchecked)
      newStaged = newStaged.filter((i) => i !== idx);
    } else {
      newStaged.push(idx);
    }
    this.setState({ staged: newStaged });
  };

  onStateUpdate = (idx: number, state: PenroseState) => {
    const newStates = [...this.state.states];
    newStates.splice(idx, 1, state);
    this.setState({
      states: newStates,
    });
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
      const synth = new Synthesizer(env, setting, subResult, "test0");
      let progs = synth.generateSubstances(numPrograms);
      const template: SubProg<A> | undefined = synth.getTemplate();
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

  exportDiagrams = async (indices: number[]) => {
    const zip = JSZip();
    zip.file(`domain.dsl`, this.state.domain);
    zip.file(`style.sty`, this.state.style);
    for (const idx of indices) {
      const state = this.state.states[idx];
      const { prog, ops } = this.state.progs[idx];
      const svg = await RenderStatic(state, async (path: string) => {
        const response = await fetch(path);
        if (!response.ok) {
          console.error(`could not fetch ${path}`);
          return undefined;
        }
        return await response.text();
      });
      zip.file(`diagram_${idx}.svg`, svg.outerHTML.toString());
      zip.file(`substance_${idx}.sub`, prettySubstance(prog));
      zip.file(`mutations_${idx}.txt`, showMutations(ops));
    }
    zip.generateAsync({ type: "blob" }).then(function (content) {
      saveAs(content, "diagrams.zip");
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
              <Button
                variant="outlined"
                color="inherit"
                onClick={() => this.exportDiagrams(this.state.staged)}
              >
                Export
              </Button>
              <Button
                variant="outlined"
                color="inherit"
                onClick={() =>
                  this.exportDiagrams(range(0, this.state.states.length))
                }
              >
                Export All
              </Button>
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
            onStateUpdate={this.onStateUpdate}
          />
        </ContentSection>
      </div>
    );
  }
}
