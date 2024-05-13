import styled from "styled-components";
import BlueButton from "./BlueButton";

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
  onClick,
  onDelete,
  onRename,
}: {
  children: React.ReactNode;
  onClick: () => void;
  onDelete: () => void;
  onRename: () => void;
}) {
  return (
    <_FileButton isFocused={false} onClick={onClick}>
      <div
        style={{
          display: "flex",
          width: "100%",
          justifyContent: "space-between",
          alignItems: "center",
          boxSizing: "border-box",
        }}
      >
        <div>{children}</div>
        <div
          style={{
            display: "flex",
            width: "100%",
            justifyContent: "right",
            alignItems: "center",
            boxSizing: "border-box",
          }}
        >
          <div>
            <BlueButton
              onClick={(e) => {
                e.stopPropagation();
                onDelete();
              }}
              title="Delete"
            >
              x
            </BlueButton>
          </div>
          <div>
            <BlueButton
              onClick={(e) => {
                e.stopPropagation();
                onRename();
              }}
              title="Rename"
            >
              ⇆
            </BlueButton>
          </div>
        </div>
      </div>
    </_FileButton>
  );
}
