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
`;
export default BlueButton;
