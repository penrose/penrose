import { PenroseState } from "@penrose/core";
import React from "react";
import styled from "styled-components";
import Checkbox from "./Checkbox";
import { Simple, SimpleProps } from "./Simple";

export type GridboxProps = SimpleProps & {
  header: string;
  gridIndex: number;
  metadata: {
    name: string;
    data: string;
  }[];
  onSelected?: (n: number) => void;
  onStateUpdate?: (n: number, s: PenroseState) => void;
};

const Section = styled.div`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border-color: ${(props) => props.theme.secondary};
  border-width: 2px;
  border-style: solid;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
`;

const Header = styled.div`
  width: calc(100% - 0.75rem);
  height: 1.75rem;
  font-size: 1.25rem;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  padding: 0.5rem 0 0.5rem 0.75rem;
  vertical-align: text-bottom;
`;

// styled(Box)(({ theme }) => ({
//   color: theme.palette.secondary.main,
//   width: "calc(100% - .75rem)",
//   height: "1.75rem",
//   borderBottom: `1px solid ${theme.palette.secondary.main}`,
//   fontSize: "1.25rem",
//   display: "flex",
//   flexDirection: "row",
//   justifyContent: "space-between",
//   padding: "0.5rem 0 0.5rem 0.75rem",
//   verticalAlign: "text-bottom",
// }));

const Body = styled.div`
  width: calc(25rem - 1rem);
  position: absolute;
  background-color: #fff;
  height: calc(100% - 1rem);
  border-radius: 5px;
  padding: 0.5rem;
  font-size: 0.8rem;
  font-family: Roboto Mono, Courier New, sans-serif;
  whitespace: pre-wrap;
  overflow: scroll;
`;

// styled(Box)({
//   width: "calc(25rem - 1rem)",
//   position: "absolute",
//   backgroundColor: "#fff",
//   fontFamily: "Roboto Mono, Courier New, sans-serif",
//   height: "calc(25rem - 4.25rem)",
//   fontSize: "0.8rem",
//   color: "black",
//   whiteSpace: "pre-wrap",
//   padding: "0.5rem 0.25rem 0.25rem 0.5rem",
// });

const H2 = styled.div`
  borderbottom: 1px solid black;
  padding: 0.5rem 0 0.35rem 0;
  marginbottom: 0.5rem;
  fontfamily: sans-serif;
  color: gray;
`;

const HeaderText = styled.div`
  color: ${(props) => props.theme.secondary};
  vertical-align: text-bottom;
  font-family: monospace;
`;

// const ExportCheckbox = styled(Checkbox)`
//   padding: 0 2.2rem";
// `;

const ResampleBtn = styled.button`
  outline: none;
  display: inline-block;
  cursor: pointer;
  text-align: center;
  vertical-align: middle;
  user-select: none;
  color: #ffffff;
  background-color: #40b4f7;
  padding: 0.25em 0.3em 0.3em 0.3em;
  margin: 0 0.3em 0 0.3em;
  user-select: none;
  border-radius: 6px;
  transition: 0.2s;
  border: none;
  font-size: 15px;
  :hover {
    background-color: #049cdd;
    transition: 0.2s;
  }
  :disabled {
    opacity: 0.5;
    pointer-events: none;
  }
`;

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
    if (this.props.onSelected) {
      this.props.onSelected(this.props.gridIndex);
    }
  };

  resample = () => {
    this.setState({ variation: Math.random().toString() });
  };

  render() {
    const { header, onSelected, onStateUpdate } = this.props;

    // const stmts = this.props.substance;
    return (
      <Section key={`gridbox-container-${this.props.gridIndex}`}>
        <Header>
          <HeaderText>{header ?? "Diagram"}</HeaderText>
          <div>
            {/* <ResampleBtn onClick={this.resample}>Resample</ResampleBtn> */}
            {onSelected && (
              <Checkbox
                checked={this.state.isSelected}
                onChange={this.checkboxClick}
              />
            )}
          </div>
        </Header>

        <div
          onClick={this.toggleView}
          style={{ height: "calc(100% - 2.5rem)", position: "relative" }}
        >
          {this.state.showDiagramInfo && (
            <Body>
              {this.props.metadata.map(({ name, data }) => (
                <div key={`gridbox-data-${name}`}>
                  <H2>{name}</H2>
                  <div
                    style={{
                      whiteSpace: "pre-line",
                    }}
                  >
                    {data}
                  </div>
                </div>
              ))}
            </Body>
          )}
          <Simple
            {...this.props}
            key={`gridbox-${this.props.gridIndex}`}
            interactive={false}
            onFrame={(state: PenroseState) => {
              this.setState({ currentState: state });
              if (onStateUpdate) {
                onStateUpdate(this.props.gridIndex, state);
              }
            }}
          />
        </div>
      </Section>
    );
  }
}
