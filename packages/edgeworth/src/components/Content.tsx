import {
  AppBar,
  Box,
  Button,
  styled,
  Toolbar,
  Typography,
} from "@material-ui/core";
import { MultipleChoiceProblem } from "@penrose/components";
import {
  compileDomain,
  compileSubstance,
  PenroseState,
  prettySubstance,
  showError,
  toSVG,
} from "@penrose/core";
import { initSubstanceEnv } from "@penrose/core/dist/compiler/Substance";
import { saveAs } from "file-saver";
import JSZip from "jszip";
import { shuffle } from "lodash";
import React, { memo } from "react";
import * as sc from "styled-components";
import { showMutations } from "../synthesis/Mutation.js";
import {
  SynthesizedSubstance,
  Synthesizer,
  SynthesizerSetting,
} from "../synthesis/Synthesizer.js";
import { Grid } from "./Grid.js";
import { Settings } from "./Settings.js";

const edgeworthPurple = {
  primary: "#3f51b5",
  correct: "#8CE363",
  incorrect: "#FA7070",
};

export type ContentProps = any;

export type SelectedDiagram = {
  index: number;
  correct: boolean;
};

export interface ContentState {
  progs: SynthesizedSubstance[];
  states: PenroseState[];
  staged: SelectedDiagram[];
  domain: string;
  style: string;
  showProblem: boolean;
  prompt: string;
  layoutDone: boolean;
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

const OutlineButton = styled(Button)({
  "&.MuiButton-outlined.Mui-disabled": {
    color: "#b7babf7e",
    borderColor: "#b7babf7e",
  },
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
      prompt: "",
      layoutDone: false,
      showProblem: false,
    };
  }
  // callback function to indicate that a svg will be exported
  addStaged = (index: number, correct: boolean) => {
    let newStaged = [...this.state.staged];
    if (this.state.staged.map(({ index }) => index).includes(index)) {
      // remove old diagram selection if it was already staged
      newStaged = newStaged.filter(
        ({ index: stagedIndex }) => stagedIndex !== index,
      );
    }
    // push the new or updated diagram to the array
    newStaged.push({ index, correct });
    this.setState({ staged: newStaged });
  };

  removeStaged = (index: number) => {
    let newStaged = this.state.staged.filter((s) => s.index !== index);
    this.setState({ staged: newStaged });
  };

  onStateUpdate = (idx: number, state: PenroseState) => {
    const newStates = [...this.state.states];
    newStates.splice(idx, 1, state);
    this.setState({
      states: newStates,
    });
  };

  onPrompt = (prompt: string) => this.setState({ prompt });

  generateProgs =
    () =>
    (
      setting: SynthesizerSetting,
      seed: string,
      numPrograms: number,
      dsl: string,
      sub: string,
      sty: string,
    ) => {
      const envOrError = compileDomain(dsl);

      // initialize synthesizer
      if (envOrError.isOk()) {
        const domEnv = envOrError.value;
        let subEnv;
        if (sub.length > 0) {
          const subRes = compileSubstance(sub, domEnv);
          if (subRes.isOk()) {
            subEnv = subRes.value;
          } else {
            console.log(
              `Error when compiling the template Substance program: ${showError(
                subRes.error,
              )}`,
            );
          }
        }
        const synth = new Synthesizer(
          domEnv,
          subEnv === undefined ? initSubstanceEnv() : subEnv,
          setting,
          subEnv === undefined ? undefined : [subEnv, domEnv],
          seed,
        );
        let progs = synth.generateSubstances(numPrograms);
        const template = synth.getTemplate();

        if (template) {
          this.setState({
            progs: [{ prog: template, ops: [] }, ...progs],
            staged: [],
            domain: dsl,
            style: sty,
            layoutDone: false,
          });
        }
      }
    };

  exportDiagrams = async (indices: number[]) => {
    const zip = JSZip();
    zip.file(`domain.domain`, this.state.domain);
    zip.file(`style.style`, this.state.style);
    for (const idx of indices) {
      const state = this.state.states[idx];
      const { prog, ops } = this.state.progs[idx];
      const svg = await toSVG(
        state,
        async (path: string) => {
          const response = await fetch(path);
          if (!response.ok) {
            console.error(`could not fetch ${path}`);
            return undefined;
          }
          return await response.text();
        },
        "diagram", // standalone SVG exports don't require distinct namespaces
      );
      zip.file(`diagram_${idx}.svg`, svg.outerHTML.toString());
      zip.file(`substance_${idx}.substance`, prettySubstance(prog));
      zip.file(`mutations_${idx}.txt`, showMutations(ops));
    }
    zip.generateAsync({ type: "blob" }).then(function (content) {
      saveAs(content, "diagrams.zip");
    });
  };

  grid = (progs: SynthesizedSubstance[]) => (
    <sc.ThemeProvider theme={edgeworthPurple}>
      <Grid
        diagrams={progs.map(({ prog }, i) => ({
          substance: prettySubstance(prog),
          style: this.state.style,
          domain: this.state.domain,
          variation: `${i}`,
        }))}
        header={(i) => (i === 0 ? "Original diagram" : `Mutated diagram #${i}`)}
        metadata={(i) => [
          {
            name: "Substance program",
            data: prettySubstance(progs[i].prog),
          },
          {
            name: "Mutations",
            data: showMutations(progs[i].ops),
          },
        ]}
        gridBoxProps={{
          stateful: true,
          animate: true,
          stepSize: 20,
        }}
        selected={this.state.staged.map(({ index }) => index)}
        onSelected={this.addStaged}
        onDeselected={this.removeStaged}
        onStateUpdate={this.onStateUpdate}
        onComplete={() => this.setState({ layoutDone: true })}
      />
    </sc.ThemeProvider>
  );

  problem = (answer: { correct: number[]; incorrect: number[] }) => {
    const { progs, domain, style, prompt } = this.state;
    const options = progs.reduce(
      (
        problems: {
          style: string;
          domain: string;
          substance: string;
          variation: string;
          answer: boolean;
        }[],
        p: SynthesizedSubstance,
        i: number,
      ) => {
        const substance = prettySubstance(p.prog);
        if (answer.correct.includes(i)) {
          return [
            ...problems,
            { substance, style, domain, variation: `${i}`, answer: true },
          ];
        } else if (answer.incorrect.includes(i)) {
          return [
            ...problems,
            {
              substance,
              style,
              domain,
              variation: `${i}`,
              answer: false,
            },
          ];
        } else return problems;
      },
      [],
    );
    return (
      <div
        style={{
          position: "fixed",
          height: "100%",
          width: "100%",
          display: "block",
          zIndex: "1201",
          visibility: this.state.showProblem ? "visible" : "hidden",
          backgroundColor: "#0000007d",
        }}
        onClick={() => this.setState({ showProblem: false })}
      >
        <div
          style={{
            display: "flex",
            width: "100%",
            justifyContent: "center",
          }}
        >
          <MultipleChoiceProblem
            diagrams={shuffle(options)}
            correctIndices={answer.correct}
            prompt={prompt}
          ></MultipleChoiceProblem>
        </div>
      </div>
    );
  };

  render() {
    const Problem = memo(
      ({ correct, incorrect }: { correct: number[]; incorrect: number[] }) =>
        this.problem({ correct, incorrect }),
    );
    const stagedCount = this.state.staged.length;
    const correctCount = this.state.staged.filter(
      ({ correct }) => correct,
    ).length;
    return (
      <div>
        <AppBar position="fixed">
          <HeaderContent>
            <Title variant="h6" noWrap>
              EDGEWORTH
            </Title>
            <ButtonBox>
              <StagedText>{`${stagedCount} diagrams selected. ${correctCount} correct and ${
                stagedCount - correctCount
              } incorrect.`}</StagedText>
              <OutlineButton
                variant="outlined"
                color="inherit"
                onClick={() =>
                  this.exportDiagrams(
                    this.state.staged.map(({ index }) => index),
                  )
                }
              >
                Export
              </OutlineButton>
              {/* <Button
                variant="outlined"
                color="inherit"
                onClick={() =>
                  this.exportDiagrams(range(0, this.state.states.length))
                }
              >
                Export All
              </Button> */}
              <OutlineButton
                disabled={!this.state.layoutDone}
                variant="outlined"
                color="inherit"
                onClick={() =>
                  this.setState(({ showProblem }) => ({
                    showProblem: !showProblem,
                  }))
                }
              >
                {this.state.showProblem ? "Hide Problem" : "Show Problem"}
              </OutlineButton>
            </ButtonBox>
          </HeaderContent>
        </AppBar>
        {/* NOTE: the Toolbar is used exclusively to space the content underneath the header of the page */}
        <Toolbar />
        <ContentSection>
          <>
            <Settings
              generateCallback={this.generateProgs()}
              onPrompt={(prompt) => this.setState({ prompt })}
              defaultDomain={this.state.domain}
              defaultStyle={this.state.style}
            />
            <Problem
              correct={this.state.staged
                .filter(({ correct }) => correct)
                .map(({ index }) => index)}
              incorrect={this.state.staged
                .filter(({ correct }) => !correct)
                .map(({ index }) => index)}
            ></Problem>
            {this.grid(this.state.progs)}
          </>
        </ContentSection>
      </div>
    );
  }
}
