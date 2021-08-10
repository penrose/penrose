import {
  compileTrio,
  evalEnergy,
  prepareState,
  RenderStatic,
  prettySubstance,
  stepUntilConvergence,
  resample,
  SynthesizedSubstance,
  showMutations,
} from "@penrose/core";
import React from "react";
import {
  styled,
  Box,
  Checkbox,
  Card,
  Typography,
  Chip,
} from "@material-ui/core";
import { IState } from "@penrose/core/build/dist/types/state";

export interface GridboxProps {
  domain: string;
  style: string;
  substance: SynthesizedSubstance;
  progNumber: number;
  srcProg: any;
  onStaged: (n: number, s: string) => void;
}

// const Section = styled.section`
//   margin: 0.5rem;
//   width: 25rem;
//   height: 25rem;
//   border: 1px solid gray;
// `;

const Section = styled(Card)(({ theme }) => ({
  margin: "0.5rem",
  width: "25rem",
  height: "25rem",
  borderColor: theme.palette.primary.main,
  borderWidth: "1px",
  borderStyle: "outset",
  color: theme.palette.primary.main,
  borderRadius: "3px",
}));

const LowEnergy = styled(Chip)(({ theme }) => ({
  background: theme.palette.success.light,
  color: "white",
}));

const HighEnergy = styled(Chip)(({ theme }) => ({
  background: theme.palette.error.light,
  color: "white",
}));

const Header = styled(Box)({
  width: "calc(100% - .75rem)",
  height: "1.75rem",
  borderBottom: "1px solid black",
  fontSize: "1.25rem",
  color: "gray",
  display: "flex",
  flexDirection: "row",
  justifyContent: "space-between",
  padding: "0.5rem 0 0.5rem 0.75rem",
  verticalAlign: "baseline",
});

const Body = styled(Box)({
  fontFamily: "Courier New, sans-serif",
  height: "calc(25rem - 4.25rem)",
  fontSize: "0.8rem",
  color: "black",
  overflow: "auto",
  whiteSpace: "pre-wrap",
  padding: "0.5rem 0.25rem 0.25rem 0.5rem",
});

const ExportCheckbox = styled(Checkbox)({
  padding: "0 0.5rem",
});

const programString = (stmts: string, ops: string) => {
  return `${stmts}
-----
Operations:
${ops}
`;
};

interface GridboxState {
  showDiagram: boolean;
  isSelected: boolean;
  diagramSVG: string;
  energy: number;
}

export class Gridbox extends React.Component<GridboxProps, GridboxState> {
  constructor(props: GridboxProps) {
    super(props);
    this.state = {
      showDiagram: true,
      isSelected: false,
      diagramSVG: "",
      energy: 0,
    };
  }

  // TODO this creates the source program state for every mutant program, should cache this information
  computeEnergy = async (optimizedState: IState) => {
    let srcState: IState;
    const resSrc = compileTrio(
      this.props.domain,
      prettySubstance(this.props.srcProg),
      this.props.style
    );
    if (resSrc.isOk()) {
      srcState = await prepareState(resSrc.value);
      const opt = stepUntilConvergence(srcState);
      if (opt.isErr()) {
        throw Error("optimization failed");
      }
      if (opt.value) {
        const crossState = {
          ...optimizedState,
          constrFns: srcState.constrFns,
          objFns: srcState.objFns,
        };

        try {
          const energy = evalEnergy(await prepareState(crossState));
          this.setState({
            energy: Math.round(energy),
          });
        } catch (e) {
          console.log(e);
        }
      }
    }
  };

  // TODO: this should really be put in a web worker, it blocks browser interaction
  async update() {
    const res = compileTrio(
      this.props.domain,
      prettySubstance(this.props.substance.prog),
      this.props.style
    );
    if (res.isOk()) {
      try {
        // https://stackoverflow.com/a/19626821
        // setTimeout causes this function to be pushed to bottom of call stack. Since Gridbox
        // component is rendered in an array, we want to delay ALL componentDidMount calls until
        // after ALL gridboxes have been initially rendered.
        await new Promise((r) => setTimeout(r, 1));
        let state = await prepareState(res.value);
        state = resample(state, 1);
        const opt = stepUntilConvergence(state);
        if (opt.isErr()) {
          throw Error("optimization failed");
        }
        const optimized = opt.value;
        this.setState({ diagramSVG: RenderStatic(optimized).outerHTML });
        this.computeEnergy(optimized);
      } catch (e) {
        console.log(e);
      }
    } else {
      throw res.error;
    }
  }

  async componentDidMount() {
    await this.update();
  }

  async componentDidUpdate(prevProps: GridboxProps) {
    if (this.props.substance.prog !== prevProps.substance.prog) {
      await this.update();
    }
  }

  toggleView = () => {
    this.setState({ showDiagram: !this.state.showDiagram });
  };

  checkboxClick = () => {
    this.setState({ isSelected: !this.state.isSelected });
    this.props.onStaged(this.props.progNumber, this.state.diagramSVG);
  };

  render() {
    const stmts = prettySubstance(this.props.substance.prog);
    return (
      <Section>
        <Header>
          <Typography>
            {this.props.progNumber === 0
              ? "Original Diagram"
              : `Mutated Program #${this.props.progNumber}`}
          </Typography>
          <Box>
            {this.state.energy > 10000 ? (
              <HighEnergy label={`energy: ${this.state.energy}`} size="small" />
            ) : (
              <LowEnergy label={`energy: ${this.state.energy}`} size="small" />
            )}
            <ExportCheckbox
              name="isStaged"
              checked={this.state.isSelected}
              onChange={this.checkboxClick}
            />
          </Box>
        </Header>

        <div onClick={this.toggleView}>
          {this.state.showDiagram ? (
            <div
              style={{ width: "100%", height: "100%" }}
              dangerouslySetInnerHTML={{
                __html: this.state.diagramSVG,
              }}
            />
          ) : (
            <Body>
              {programString(
                stmts,
                this.props.progNumber === 0
                  ? ""
                  : showMutations(this.props.substance.ops)
              )}
            </Body>
          )}
        </div>
      </Section>
    );
  }
}
