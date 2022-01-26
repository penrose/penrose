import styled from "styled-components";

const StyledFileButton = styled.button`
  cursor: pointer;
  display: block;
  margin: 10px;
  padding: 10px;
  border: none;
  border-radius: 10px;
  transition: background-color 0.2s ease-in-out;
  background-color: #f0f0f0;
  :hover {
    background-color: #d4d4d4;
    transition: background-color 0.2s ease-in-out;
  }
`;
export default function FileButton({
  onClick,
  name,
}: {
  onClick(): void;
  name: string;
}) {
  return (
    <StyledFileButton onClick={onClick}>
      <span>{name}</span>
    </StyledFileButton>
  );
}
