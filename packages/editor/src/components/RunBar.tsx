import { FileDispatcher } from "../state/fileReducer";
import {
  FilePointer,
  FilePointerMap,
  IWorkspace,
  TrioType,
} from "../types/FileSystem";
import BlueButton from "./BlueButton";
import Select from "react-select";
import styled from "styled-components";

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

const mapFilterFiles = (openFiles: FilePointerMap, fileType: TrioType) =>
  Object.values(openFiles).filter(({ type }) => type === fileType);

const FileSelector = ({
  workspace,
  fileType,
  dispatch,
}: {
  workspace: IWorkspace;
  dispatch: FileDispatcher;
  fileType: TrioType;
}) => {
  const openFiles = mapFilterFiles(workspace.openFiles, fileType);
  if (!workspace.compileTrioSetting[fileType]) {
    return <div style={{ color: "#666666" }}>no {fileType}</div>;
  }
  return (
    <Select
      aria-label={`Select ${fileType} file`}
      options={openFiles}
      getOptionLabel={({ name }: FilePointer) => name}
      getOptionValue={({ id }: FilePointer) => id}
      name={fileType}
      value={
        workspace.openFiles[workspace.compileTrioSetting[fileType] as string]
      }
      onChange={(v) =>
        dispatch({ type: "SET_TRIO_MEMBER", id: v!.id, kind: fileType })
      }
      noOptionsMessage={() => `No ${fileType}s`}
      isSearchable
    />
  );
};

export default function RunBar({
  compile,
  workspace,
  dispatch,
}: {
  compile(): void;
  workspace: IWorkspace;
  dispatch: FileDispatcher;
}) {
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
        <FileSelector
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
        </CompileButton>
      </div>
    </nav>
  );
}
