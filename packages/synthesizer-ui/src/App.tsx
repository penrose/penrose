import { domainProg, subProg, styProg } from "./trio";
import {
  compileDomain,
  compileSubstance,
  compileTrio,
  prepareState,
  prettySubstance,
  RenderInteractive,
  RenderStatic,
  showError,
  stepState,
  stepUntilConvergence,
  SubProg,
  Synthesizer,
  SynthesizerSetting,
} from "@penrose/core";
import { GridList, GridListTile } from "@material-ui/core";
import React from "react";

export const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  argReuse: "distinct",
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

const synthesizeProgs = (
  substanceSrc: string,
  styleSrc: string,
  domainSrc: string,
  settings: SynthesizerSetting,
  numPrograms: number
) => {
  // initialize synthesizer

  const envOrError = compileDomain(domainSrc);

  if (envOrError.isOk()) {
    const env = envOrError.value;
    let subResult;
    const subRes = compileSubstance(substanceSrc, env);
    if (subRes.isOk()) {
      subResult = subRes.value;
    } else {
      console.log(
        `Error when compiling the template Substance program: ${showError(
          subRes.error
        )}`
      );
    }
    const synth = new Synthesizer(env, settings, subResult);
    let progs = synth.generateSubstances(numPrograms);
    const template: SubProg | undefined = synth.getTemplate();

    if (template) {
      progs = [{ prog: template }, ...progs];
    }
    return progs;
  } else {
    console.log(
      `Error when compiling the domain program:\n${showError(envOrError.error)}`
    );
  }
};

interface DiagramProps {
  state: any;
  key: string;
}

class Diagram extends React.Component<any, DiagramProps> {
  constructor(props: any) {
    super(props);
  }

  render() {
    return (
      <div
        style={{ width: "200px", height: "200px" }}
        dangerouslySetInnerHTML={{
          __html: RenderStatic(this.props.state).outerHTML,
        }}
      >
        {/* {RenderInteractive(this.props.state)} */}
      </div>
    );
  }
}

const toGrid = (diagrams: any[]) => (
  <GridList cellHeight={160} className="grid" cols={5}>
    {diagrams.map((d: any, n: number) => (
      <GridListTile key={d.key} cols={1}>
        <Diagram state={stepUntilConvergence(d)} key={`diagram-${n}`} />
      </GridListTile>
    ))}
  </GridList>
);

class App extends React.Component {
  state = {
    diagrams: [],
  };

  componentDidMount = async () => {
    this.setState({ diagrams: await this.renderDiagrams() });
  };

  renderDiagrams = async () => {
    const progs = synthesizeProgs(
      subProg,
      styProg,
      domainProg,
      defaultSetting,
      30
    );

    const diagramsOrErrors = progs.map(({ prog }: any) =>
      compileTrio(domainProg, prettySubstance(prog), styProg)
    );

    const diagrams = diagramsOrErrors
      .filter((s: any) => s.isOk())
      .map(async (s: any) => {
        return prepareState(s.value);
        // return stepUntilConvergence(s.value);
      });
    return Promise.all(diagrams);
  };

  render() {
    return <div className="App">{toGrid(this.state.diagrams)}</div>;
  }
}

export default App;
