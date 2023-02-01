import { Box, styled, Typography } from "@material-ui/core";
import { PenroseState } from "@penrose/core";
import React from "react";
import { Gridbox } from "./Gridbox";

type DiagramSource = {
  style: string;
  domain: string;
  substance: string;
  variation: string;
};

export interface GridProps {
  diagrams: DiagramSource[];
  onSelected: (n: number) => void;
  onStateUpdate: (n: number, s: PenroseState) => void;
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

export class Grid extends React.Component<GridProps> {
  constructor(props: GridProps) {
    super(props);
  }

  setSrcState = (newState: PenroseState) => {
    this.setState({
      srcState: newState,
    });
  };

  innerContent() {
    return this.props.diagrams.map(
      ({ substance, domain, style, variation }, i) => (
        <Gridbox
          key={`grid-${i}`}
          header={`Diagram ${i}`}
          metadata={[
            {
              name: "Variation",
              data: variation,
            },
          ]}
          domain={domain}
          style={style}
          gridIndex={i}
          substance={substance}
          variation={variation}
          onSelected={this.props.onSelected}
          onStateUpdate={this.props.onStateUpdate}
        />
      )
    );
  }

  render() {
    const content =
      this.props.diagrams.length === 0 ? (
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
