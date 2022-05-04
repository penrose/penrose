import {
  Box,
  Button,
  Card,
  Checkbox,
  Chip,
  styled,
  Typography,
} from "@material-ui/core";
import { fetchResolver } from "@penrose/components";
import {
  compileTrio,
  evalEnergy,
  PenroseState,
  prepareState,
  prettySubstance,
  RenderStatic,
  resample,
  showError,
  showMutations,
  stepUntilConvergence,
  SynthesizedSubstance,
} from "@penrose/core";
import React from "react";

export interface GridboxProps {
  domain: string;
  style: string;
  substance: SynthesizedSubstance;
  variation: string;
  progNumber: number;
  srcState: PenroseState | undefined;
  updateSrcProg: (newState: PenroseState) => void;
  onStaged: (n: number, s: string) => void;
}

const Section = styled(Card)(({ theme }) => ({
  margin: "0.5rem",
  width: "25rem",
  height: "25rem",
  borderColor: theme.palette.primary.main,
  borderWidth: "2px",
  borderStyle: "outset",
  color: theme.palette.primary.main,
  borderRadius: "5px",
}));

const LowEnergy = styled(Chip)(({ theme }) => ({
  background: theme.palette.success.main,
  color: "white",
}));

const HighEnergy = styled(Chip)(({ theme }) => ({
  background: theme.palette.error.light,
  color: "white",
}));

const Header = styled(Box)(({ theme }) => ({
  color: theme.palette.primary.main,
  width: "calc(100% - .75rem)",
  height: "1.75rem",
  borderBottom: "1px solid black",
  fontSize: "1.25rem",
  display: "flex",
  flexDirection: "row",
  justifyContent: "space-between",
  padding: "0.5rem 0 0.5rem 0.75rem",
  verticalAlign: "text-bottom",
}));

const Body = styled(Box)({
  fontFamily: "Roboto Mono, Courier New, sans-serif",
  height: "calc(25rem - 4.25rem)",
  fontSize: "0.8rem",
  color: "black",
  overflow: "auto",
  whiteSpace: "pre-wrap",
  padding: "0.5rem 0.25rem 0.25rem 0.5rem",
});

const H2 = styled(Box)({
  borderBottom: "1px solid black",
  padding: "0.5rem 0 0.35rem 0",
  marginBottom: ".5rem",
  fontFamily: "sans-serif",
  color: "gray",
});

const HeaderText = styled(Typography)(({ theme }) => ({
  color: theme.palette.primary.main,
  fontFamily: "Roboto Mono, Helvetica, sans-serif",
  verticalAlign: "text-bottom",
}));

const ExportCheckbox = styled(Checkbox)({
  padding: "0 0.5rem",
});

const ResampleBtn = styled(Button)({
  fontSize: "0.8rem",
  padding: "0 0.5rem",
});

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

  computeEnergy = async (optimizedState: PenroseState) => {
    if (this.props.srcState) {
      const crossState = {
        ...optimizedState,
        constrFns: this.props.srcState.constrFns,
        objFns: this.props.srcState.objFns,
      };

      try {
        // resample because initial sampling did not use the special sampling seed
        const energy = evalEnergy(await prepareState(crossState));
        this.setState({
          energy: Math.round(energy),
        });
      } catch (e) {
        console.log("error with CIEE: ", e);
        this.setState({
          energy: -1,
        });
      }
    }
  };

  // TODO: this should really be put in a web worker, it blocks browser interaction
  async update() {
    const res = compileTrio({
      substance: prettySubstance(this.props.substance.prog),
      style: this.props.style,
      domain: this.props.domain,
      variation: this.props.variation,
    });
    if (res.isOk()) {
      try {
        // https://stackoverflow.com/a/19626821
        // setTimeout causes this function to be pushed to bottom of call stack. Since Gridbox
        // component is rendered in an array, we want to delay ALL componentDidMount calls until
        // after ALL gridboxes have been initially rendered.
        await new Promise((r) => setTimeout(r, 1));
        // resample because initial sampling did not use the special sampling seed
        let state = resample(await prepareState(res.value));
        state = resample(state);
        const opt = stepUntilConvergence(state);
        if (opt.isErr()) {
          console.log(showError(opt.error));
          throw Error("optimization failed");
        }
        const optimized = opt.value;
        const rendered = await RenderStatic(optimized, fetchResolver);
        this.setState({ diagramSVG: rendered.outerHTML });
        if (this.props.progNumber === 0) {
          // original program is cached by parent component to be used for CIEE with mutated progs
          this.props.updateSrcProg(optimized);
        } else {
          this.computeEnergy(optimized);
        }
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

  resample = () => {
    this.update();
  };

  // NOTE: not rendered by default, uncomment in render function to see
  energyChip = () => {
    return this.state.energy > 10000 || this.state.energy < 0 ? (
      <HighEnergy
        label={`energy: ${
          this.state.energy < 0 ? "Inf" : this.state.energy.toExponential(2)
        }`}
        size="small"
      />
    ) : (
      <LowEnergy label={`energy: ${this.state.energy}`} size="small" />
    );
  };

  render() {
    const stmts = prettySubstance(this.props.substance.prog);
    return (
      <Section>
        <Header>
          <HeaderText>
            {this.props.progNumber === 0
              ? "Original Diagram"
              : `Mutated Program #${this.props.progNumber}`}
          </HeaderText>
          <Box>
            <ResampleBtn
              onClick={this.resample}
              variant="contained"
              color="primary"
            >
              Resample
            </ResampleBtn>
            {/* {this.energyChip()} */}
            <ExportCheckbox
              name="isStaged"
              checked={this.state.isSelected}
              onChange={this.checkboxClick}
              color="primary"
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
              <H2>Mutations</H2>
              {this.props.progNumber === 0
                ? "N/A"
                : showMutations(this.props.substance.ops)}
              <H2>Substance Program</H2>
              {`${stmts}`}
            </Body>
          )}
        </div>
      </Section>
    );
  }
}
