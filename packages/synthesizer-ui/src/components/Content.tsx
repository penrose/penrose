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
import { Header } from "./Header";

export type ContentProps = any;

export interface SynthesizedSubstance {
  prog: SubProg;
  ops: string;
}

export interface ContentState {
  setting: SynthesizerSetting;
  progs: SynthesizedSubstance[];
  style: string;
  domain: string;
}

const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.2,
  },
  add: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["Equal"],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
  },
  edit: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
  },
};

export class Content extends React.Component<ContentProps, ContentState> {
  constructor(props: ContentProps) {
    super(props);
    this.state = {
      setting: defaultSetting,
      progs: [],
      style: "",
      domain: "",
    };
  }

  updateSettings = (newSetting: SynthesizerSetting) => {
    this.setState({
      setting: newSetting,
    });
  };

  generateProgs = () => {
    const envOrError = compileDomain(this.state.domain);

    // initialize synthesizer
    if (envOrError.isOk()) {
      const env = envOrError.value;
      let subResult;
      if (this.props.substance.length > 0) {
        const subRes = compileSubstance(this.props.substance, env);
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

      const synth = new Synthesizer(env, this.props.settings, subResult);
      let progs = synth.generateSubstances(this.state.setting.numPrograms);
      const template: SubProg | undefined = synth.getTemplate();

      if (template) {
        progs = [{ prog: template }, ...progs];
      }
      this.setState({ progs });
    }
    this.setState({ progs: [] });
  };

  render() {
    return (
      <div>
        <Header />
        <Grid
          style={this.state.style}
          domain={this.state.domain}
          progs={this.state.progs}
        />
      </div>
    );
  }
}
