import styled from "styled-components";

const _FileButton = styled.button<{ isFocused: boolean }>`
  background-color: ${({ isFocused }) => (isFocused ? "#40b4f7" : "#ffffff")};
  color: ${({ isFocused }) => (isFocused ? "#FFFFFF" : "#4d4d4d")};
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
  width: 100%;
  border: 0;
  text-align: left;
  font-size: 15px;
  margin: 3px;
  padding: 10px;
  transition: 0.2s;
  cursor: pointer;
  .delete {
    visibility: hidden;
  }
  :hover {
    transition: 0.2s;
    background-color: ${({ isFocused }) =>
      isFocused ? "#40b4f7" : "rgba(0,0,0,0.1)"};
    .delete {
      visibility: visible;
    }
  }
`;

export default function FileButton({
  children,
  isFocused,
  onClick,
  onDelete,
}: {
  children: React.ReactNode;
  isFocused?: boolean;
  onClick: () => void;
  onDelete?: () => void;
}) {
  return (
    <_FileButton isFocused={isFocused ?? false} onClick={onClick}>
      <div>{children}</div>
      {onDelete && (
        <button
          className="delete"
          onClick={(e) => {
            e.stopPropagation();
            onDelete();
          }}
        >
          x
        </button>
      )}
    </_FileButton>
  );
}
