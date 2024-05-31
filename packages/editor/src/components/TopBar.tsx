import { useCallback, useState } from "react";
import { useRecoilCallback, useRecoilValue } from "recoil";
import styled from "styled-components";
import {
  currentWorkspaceState,
  diagramWorkerState,
  settingsState,
} from "../state/atoms.js";
import {
  useCompileDiagram,
  useDownloadSvg,
  useIsUnsaved,
  useNewWorkspace,
  usePublishGist,
  useResampleDiagram,
  useSaveNewWorkspace,
  useSaveWorkspace,
} from "../state/callbacks.js";
import BlueButton from "./BlueButton.js";
import ExportButton from "./ExportButton.js";

const UnsavedIcon = styled.div`
  background-color: #dddddd;
  padding: 2px 4px;
  border-radius: 2px;
  font-size: 10px;
  color: #111111;
`;

const TitleBox = styled.div`
  padding: 5px 10px;
  cursor: text;
  box-sizing: border-box;
  border: 1px solid rgba(0, 0, 0, 0);
  font-size: 15px;
  :hover {
    border: 1px solid gray;
    border-radius: 5px;
  }
`;

const AuthorBox = styled.div`
  border-radius: 5px;
  transition: 0.2s;
  display: flex;
  flex-direction: row;
  align-items: center;
  margin: 2px;
  padding: 2px 5px;
  :hover {
    transition: 0.2s;
    background-color: rgba(0, 0, 0, 0.1);
  }
`;

const InputBox = styled.input`
  padding: 5px 10px;
  cursor: text;
  box-sizing: border-box;
  border: 1px solid rgba(0, 0, 0, 0);
  font-size: 15px;
`;

const HeaderButtonContainer = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
`;

function EditableTitle() {
  const [editing, setEditing] = useState(false);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const saveWorkspace = useSaveWorkspace();
  // const saveLocally = useSaveLocally();
  const onChange = useRecoilCallback(({ set }) =>
    // Set locally
    (e: React.ChangeEvent<HTMLInputElement>) => {
      set(currentWorkspaceState, (state) => ({
        ...state,
        metadata: {
          ...state.metadata,
          name: e.target.value,
        },
      }));
    },
  );

  const onFinish = () => {
    if (currentWorkspace.metadata.location.kind == "stored") {
      saveWorkspace();
    }
    setEditing(false);
  };

  const onKey = useCallback((e: React.KeyboardEvent) => {
    if (e.key === "Enter" || e.key === "Escape") {
      if (currentWorkspace.metadata.location.kind == "stored") {
        saveWorkspace();
      }
      setEditing(false);
    }
  }, []);
  if (editing) {
    return (
      <InputBox
        type="text"
        value={currentWorkspace.metadata.name}
        autoFocus={true}
        onFocus={(e) => e.target.select()}
        onBlur={onFinish}
        onKeyDown={onKey}
        onChange={onChange}
      />
    );
  }
  return (
    <TitleBox onClick={() => setEditing(true)}>
      {currentWorkspace.metadata.name}
    </TitleBox>
  );
}

export default function TopBar() {
  const compileDiagram = useCompileDiagram();
  const resampleDiagram = useResampleDiagram();
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const settings = useRecoilValue(settingsState);
  const publishGist = usePublishGist();
  const { running } = useRecoilValue(diagramWorkerState);
  const isUnsaved = useIsUnsaved();
  const newWorkspace = useNewWorkspace();
  const saveWorkspace = useSaveWorkspace();
  const saveNewWorkspace = useSaveNewWorkspace();

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
      {currentWorkspace.metadata.location.kind === "roger" ? (
        <div>loaded from filesystem via Roger</div>
      ) : (
        <div
          style={{
            display: "flex",
            flexDirection: "row",
            alignItems: "center",
          }}
        >
          <EditableTitle />
          {isUnsaved() ? <UnsavedIcon>unsaved</UnsavedIcon> : ""}
          {currentWorkspace.metadata.location.kind === "gist" && (
            <a
              style={{ textDecoration: "none", color: "inherit" }}
              href={`https://github.com/${currentWorkspace.metadata.location.author}`}
            >
              <AuthorBox>
                <div
                  style={{
                    width: "25px",
                    height: "25px",
                    margin: "5px",
                    backgroundImage: `url(${currentWorkspace.metadata.location.avatar})`,
                    borderRadius: "50%",
                    backgroundSize: "cover",
                    display: "inline-block",
                  }}
                />{" "}
                {currentWorkspace.metadata.location.author}
              </AuthorBox>
            </a>
          )}
          {currentWorkspace.metadata.location.kind == "local" && (
            <BlueButton
              onClick={() => saveNewWorkspace(currentWorkspace.metadata.id)}
            >
              save
            </BlueButton>
          )}
          {currentWorkspace.metadata.location.kind === "stored" &&
            !currentWorkspace.metadata.location.saved && (
              <BlueButton onClick={() => saveWorkspace()}>save</BlueButton>
            )}
          {currentWorkspace.metadata.location.kind === "stored" &&
            currentWorkspace.metadata.location.saved &&
            settings.githubAccessToken !== null && (
              <BlueButton onClick={publishGist}>share</BlueButton>
            )}

          <BlueButton onClick={newWorkspace}>new workspace</BlueButton>
        </div>
      )}
      <HeaderButtonContainer>
        <BlueButton disabled={running} onClick={useDownloadSvg()}>
          save Penrose SVG
        </BlueButton>
        <ExportButton />
        <BlueButton disabled={running} onClick={compileDiagram}>
          compile ▶
        </BlueButton>
        <BlueButton disabled={running} onClick={resampleDiagram}>
          resample
        </BlueButton>
      </HeaderButtonContainer>
    </nav>
  );
}
