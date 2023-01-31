import { Box, styled, Typography } from "@material-ui/core";
import { PenroseState, SynthesizedSubstance } from "@penrose/core";
import React from "react";
import { Gridbox } from "./Gridbox";

export interface GridProps {
  style: string;
  domain: string;
  progs: SynthesizedSubstance[];
  onStaged: (n: number) => void;
  onStateUpdate: (n: number, s: PenroseState) => void;
}

interface GridState {
  srcState: PenroseState | undefined;
}

const GridContainer = styled("main")({
  flexGrow: 1,
  marginLeft: "4rem",
});

const GridContent = styled(Box)({
  display: "flex",
  flexDirection: "row",
  flexWrap: "wrap",
  justifyContent: "flex-start",
  height: "calc(100vh - 5rem)",
  width: "100%",
  overflow: "auto",
  margin: "0",
  padding: "0",
  paddingTop: "1rem",
});

const Placeholder = styled(Box)({
  display: "flex",
  justifyContent: "center",
  alignItems: "center",
  width: "100%",
  margin: "0",
  padding: "0",
});

const PlaceholderText = styled(Typography)(({ theme }) => ({
  color: theme.palette.primary.main,
  fontFamily: "Roboto Mono",
}));

export class Grid extends React.Component<GridProps, GridState> {
  constructor(props: GridProps) {
    super(props);
    this.state = {
      srcState: undefined,
    };
  }

  setSrcState = (newState: PenroseState) => {
    this.setState({
      srcState: newState,
    });
  };

  innerContent() {
    return this.props.progs.map((s, i) => (
      <Gridbox
        key={`grid-${i}`}
        domain={this.props.domain}
        style={this.props.style}
        progNumber={i}
        substance={s}
        variation={i.toString()}
        updateSrcProg={this.setSrcState}
        srcState={this.state.srcState}
        onStaged={this.props.onStaged}
        onStateUpdate={this.props.onStateUpdate}
      />
    ));
  }

  render() {
    const content =
      this.props.progs.length === 0 ? (
        <Placeholder>
          <PlaceholderText variant="h6">
            {"(Generated diagrams will appear here)"}
          </PlaceholderText>
        </Placeholder>
      ) : (
        this.innerContent()
      );
    return (
      <GridContainer>
        <GridContent>{content}</GridContent>
      </GridContainer>
    );
  }
}
