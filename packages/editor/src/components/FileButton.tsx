import styled from "styled-components";

const FileButton = styled.button<{ isFocused: boolean }>`
  background-color: ${({ isFocused }) => (isFocused ? "#40b4f7" : "#ffffff")};
  color: ${({ isFocused }) => (isFocused ? "#FFFFFF" : "#4d4d4d")};
  display: block;
  width: 100%;
  border: 0;
  text-align: left;
  font-size: 15px;
  margin: 3px;
  padding: 10px;
  transition: 0.2s;
  cursor: pointer;
  :hover {
    transition: 0.2s;
    background-color: ${({ isFocused }) =>
      isFocused ? "#40b4f7" : "rgba(0,0,0,0.1)"};
  }
`;

export default FileButton;
