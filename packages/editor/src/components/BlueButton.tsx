import styled from "styled-components";

const BlueButton = styled.button<{}>`
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
export default BlueButton;

export const BigBlueButton = styled(BlueButton)`
  padding: 10px 20px;
  margin: 10px;
  font-size: 23px;
  :disabled {
    cursor: default;
    opacity: 0.5;
  }
`;

export const SquareBlueButton = styled(BlueButton)`
  width: 20px;
  height: 20px;
  padding: 0;
  border: 1px solid #40b4f7;
  background-color: rgba(0, 0, 0, 0);
  color: #40b4f7;
  :hover {
    color: #ffffff;
    background-color: #40b4f7;
  }
`;
