import styled from "styled-components";
import { IWorkspace } from "../types/FileSystem";

const CompileButton = styled.button`
  background-color: #40b4f7;
  color: #ffffff;
  width: 40px;
  height: 40px;
  border-radius: 100px;
  border: none;
  cursor: pointer;
  :hover {
    background-color: #049cdd;
    transition: 0.2s;
  }
  :disabled {
    cursor: default;
    opacity: 0.5;
  }
`;

export default function RunBar({ workspace }: { workspace: IWorkspace }) {
  return (
    <nav
      style={{
        display: "flex",
        width: "100%",
        backgroundColor: "#F4F4F4",
        justifyContent: "space-between",
        alignItems: "center",
        padding: "10px",
        boxSizing: "border-box",
      }}
    >
      {/* TODO: sign in button/avatar */}
      <div>Penrose {workspace.name}</div>
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          gap: "10px",
          alignItems: "center",
        }}
      >
        {/* <FileSelector
          fileType={"substance"}
          workspace={workspace}
          dispatch={dispatch}
        />
        <FileSelector
          fileType={"style"}
          workspace={workspace}
          dispatch={dispatch}
        />
        <FileSelector
          fileType={"domain"}
          workspace={workspace}
          dispatch={dispatch}
        />
        <CompileButton
          disabled={
            Object.values(workspace.compileTrioSetting).filter(
              (v) => v === null
            ).length > 0
          }
        >
          {"▶"}
        </CompileButton> */}
      </div>
    </nav>
  );
}
