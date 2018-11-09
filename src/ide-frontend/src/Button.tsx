import * as React from "react";
import styled from "styled-components";

interface IProps {
  label: string;
  primary?: boolean;
  onClick(): void;
}

const B = styled.div`
  display: inline-block;
  background-color: ${({ primary }: any) => (primary ? "#40B4F7" : "#ffffff")};
  color: ${({ primary }: any) => (primary ? "#FFFFFF" : "#40b4f7")};
  border: 0.2ex solid #40b4f7;
  border-radius: 6px;
  box-sizing: border-box;
  padding: 0.15em 0.3em 0.15em 0.3em;
  margin: 0 0.3em 0 0.3em;
  font-weight: ${({ primary }: any) => (primary ? "regular" : "bold")};
  font-size: 1em;
  user-select: none;
  cursor: pointer;
  transition: 0.2s;
  :hover {
    background-color: ${({ primary }: any) =>
      primary ? "#339EDB" : "#e5f4ff"};
    border: ${({ primary }: any) =>
      primary ? "0.2ex solid #339EDB" : "0.2ex solid #40b4f7"};
    transition: 0.2s;
  }
`;

class Button extends React.Component<IProps> {
  public render() {
    return (
      <B onClick={this.props.onClick} primary={this.props.primary}>
        {this.props.label}
      </B>
    );
  }
}

export default Button;
