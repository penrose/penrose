import React from "react";
import { SynthesizedSubstance } from "@penrose/core";
import { Gridbox } from "./Gridbox";
import { Box, styled, Toolbar, Typography } from "@material-ui/core";

export interface GridProps {
  style: any;
  domain: any;
  progs: SynthesizedSubstance[];
  onStaged: (n: number, s: string) => void;
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

interface GridState {
  svgs: string[];
}

export class Grid extends React.Component<GridProps, GridState> {
  private svgs: string[];
  constructor(props: GridProps) {
    super(props);
    this.state = { svgs: [] };
    this.svgs = [];
  }

  innerContent() {
    return this.props.progs.map((s, i) => (
      <Gridbox
        key={`grid-${i}`}
        domain={this.props.domain}
        style={this.props.style}
        progNumber={i}
        substance={s}
        srcProg={this.props.progs[0].prog}
        onStaged={this.props.onStaged}
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
