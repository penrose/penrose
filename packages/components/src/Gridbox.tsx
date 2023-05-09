import { PenroseState } from "@penrose/core";
import React, { SVGProps } from "react";
import styled from "styled-components";
import Checkbox from "./Checkbox";
import { Simple, SimpleProps } from "./Simple";

export type GridboxProps = SimpleProps & {
  header: string;
  gridIndex: number;
  stateful?: boolean;
  selected?: boolean;
  metadata: {
    name: string;
    data: string;
  }[];
  onSelected?: (n: number) => void;
  onStateUpdate?: (n: number, s: PenroseState) => void;
};

const Check = styled(Checkbox)`
  margin-right: 1rem;
  margin-left: 1rem;
`;

const Svg = styled.svg.attrs({
  version: "1.1",
  xmlns: "http://www.w3.org/2000/svg",
  xmlnsXlink: "http://www.w3.org/1999/xlink",
})``;

const RefreshIcon = ({ className }: SVGProps<SVGSVGElement>) => (
  <Svg
    focusable="false"
    aria-hidden="true"
    viewBox="0 0 24 24"
    className={className}
  >
    <path d="M17.65 6.35C16.2 4.9 14.21 4 12 4c-4.42 0-7.99 3.58-7.99 8s3.57 8 7.99 8c3.73 0 6.84-2.55 7.73-6h-2.08c-.82 2.33-3.04 4-5.65 4-3.31 0-6-2.69-6-6s2.69-6 6-6c1.66 0 3.14.69 4.22 1.78L13 11h7V4l-2.35 2.35z"></path>
  </Svg>
);

const Refresh = styled(RefreshIcon)`
  user-select: none;
  width: 25px;
  height: 25px;
  display: inline-block;
  fill: currentcolor;
  transition: fill 200ms cubic-bezier(0.4, 0, 0.2, 1) 0ms;
  color: #fff;
`;

const Section = styled.div`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border-color: ${(props) => props.theme.primary};
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
  color: ${(props) => props.theme.primary};
`;

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

const H2 = styled.div`
  borderbottom: 1px solid black;
  padding: 0.5rem 0 0.35rem 0;
  marginbottom: 0.5rem;
  fontfamily: sans-serif;
  color: gray;
`;

const HeaderText = styled.div`
  color: ${(props) => props.theme.primary};
  vertical-align: text-bottom;
  font-family: monospace;
`;

const ResampleBtn = styled.button`
  display: inline-block;
  cursor: pointer;
  height: 26px;
  width: 26px;
  user-select: none;
  color: #ffffff;
  background-color: #3f51b5;
  margin: 0;
  padding: 0;
  user-select: none;
  border-radius: 6px;
  transition: 0.2s;
  border: none;
  outline: none;
  display: inline-block;
  cursor: pointer;
  text-align: center;
  vertical-align: middle;
  user-select: none;
  background-color: ${(props) => props.theme.primary};
  :hover {
    filter: brightness(70%);
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
  variation: string;
  substance: string;
  style: string;
  domain: string;
  currentState?: PenroseState;
}

export class Gridbox extends React.Component<GridboxProps, GridboxState> {
  constructor(props: GridboxProps) {
    super(props);
    this.state = {
      substance: props.substance,
      style: props.style,
      domain: props.domain,
      showDiagramInfo: false,
      isSelected: this.props.selected ?? false,
      currentState: undefined,
      variation: props.variation,
    };
  }

  componentDidUpdate(
    prevProps: Readonly<GridboxProps>,
    prevState: Readonly<GridboxState>
  ): void {
    if (this.props.selected !== prevState.isSelected) {
      this.setState({ isSelected: this.props.selected ?? false });
    }
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
    const { header, stateful, onSelected, onStateUpdate } = this.props;
    const variation = stateful ? this.state.variation : this.props.variation;

    return (
      <Section key={`gridbox-container-${this.props.gridIndex}`}>
        <Header>
          <HeaderText>{header ?? "Diagram"}</HeaderText>
          <div style={{ display: "flex" }}>
            {this.props.stateful && (
              <ResampleBtn onClick={this.resample}>
                <Refresh />
              </ResampleBtn>
            )}
            {onSelected && (
              <Check
                checked={this.state.isSelected}
                value={""}
                name={""}
                label={""}
                id={`checkbox-${this.props.gridIndex}`}
                disabled={false}
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
            variation={variation}
            key={`gridbox-${this.props.gridIndex}`}
            name={`gridbox-${this.props.gridIndex}`}
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
