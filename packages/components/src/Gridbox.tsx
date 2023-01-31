import { Box, Button, Checkbox, styled, Typography } from "@material-ui/core";
import { Simple } from "@penrose/components";
import { PenroseState } from "@penrose/core";
import React from "react";

export interface GridboxProps {
  domain: string;
  style: string;
  substance: string;
  variation: string;
  header: string;
  gridIndex: number;
  metadata: {
    name: string;
    data: string;
  }[];
  onSelected?: (n: number) => void;
  onStateUpdate: (n: number, state: PenroseState) => void;
}

const Section = styled(Box)(({ theme }) => ({
  margin: "0.5rem",
  width: "25rem",
  height: "25rem",
  borderColor: theme.palette.secondary.main,
  borderWidth: "2px",
  borderStyle: "solid",
  color: theme.palette.secondary.main,
  borderRadius: "5px",
  display: "flex",
  flexDirection: "column",
}));

const Header = styled(Box)(({ theme }) => ({
  color: theme.palette.secondary.main,
  width: "calc(100% - .75rem)",
  height: "1.75rem",
  borderBottom: `1px solid ${theme.palette.secondary.main}`,
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
  color: theme.palette.secondary.main,
  fontFamily: "Roboto Mono, Helvetica, sans-serif",
  verticalAlign: "text-bottom",
}));

const ExportCheckbox = styled(Checkbox)({
  padding: "0 0.5rem",
});

const ResampleBtn = styled(Button)(({ theme }) => ({
  fontSize: "0.8rem",
  padding: "0 0.5rem",
  marginRight: ".5rem",
}));

interface GridboxState {
  showDiagramInfo: boolean;
  isSelected: boolean;
  energy: number;
  variation: string;
  currentState?: PenroseState;
}

export class Gridbox extends React.Component<GridboxProps, GridboxState> {
  constructor(props: GridboxProps) {
    super(props);
    this.state = {
      showDiagramInfo: false,
      isSelected: false,
      currentState: undefined,
      energy: 0,
      variation: props.variation,
    };
  }

  toggleView = () => {
    this.setState({ showDiagramInfo: !this.state.showDiagramInfo });
  };

  checkboxClick = () => {
    this.setState({ isSelected: !this.state.isSelected });
    if (this.state.currentState && this.props.onSelected) {
      this.props.onSelected(this.props.gridIndex);
    }
  };

  resample = () => {
    this.setState({ variation: Math.random().toString() });
  };

  render() {
    const { header, onSelected } = this.props;
    // const stmts = this.props.substance;
    return (
      <Section>
        <Header>
          <HeaderText>{header ?? "Diagram"}</HeaderText>
          <Box>
            <ResampleBtn
              onClick={this.resample}
              variant="contained"
              color="primary"
            >
              Resample
            </ResampleBtn>
            {onSelected && (
              <ExportCheckbox
                name="isStaged"
                checked={this.state.isSelected}
                onChange={this.checkboxClick}
                color="primary"
              />
            )}
          </Box>
        </Header>

        <div onClick={this.toggleView} style={{ height: "100%" }}>
          {this.state.showDiagramInfo && (
            <Body>
              {this.props.metadata.map(({ name, data }) => (
                <>
                  <H2>{name}</H2>
                  <Body>{data}</Body>
                </>
              ))}
            </Body>
          )}
          <Simple
            domain={this.props.domain}
            substance={this.props.substance}
            style={this.props.style}
            variation={this.state.variation}
            interactive={false}
            animate={true}
            stepSize={20}
            onFrame={(state: PenroseState) => {
              this.setState({ currentState: state });
              this.props.onStateUpdate(this.props.gridIndex, state);
            }}
          />
        </div>
      </Section>
    );
  }
}
