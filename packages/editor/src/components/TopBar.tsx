import { useCallback, useState } from "react";
import { useRecoilCallback, useRecoilValue } from "recoil";
import styled from "styled-components";
import {
  settingsState,
  WorkspaceMetadata,
  workspaceMetadataSelector,
} from "../state/atoms";
import {
  useCompileDiagram,
  usePublishGist,
  useResampleDiagram,
  useSaveLocally,
} from "../state/callbacks";
import BlueButton from "./BlueButton";

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

function EditableTitle() {
  const [editing, setEditing] = useState(false);
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const saveLocally = useSaveLocally();
  const onChange = useRecoilCallback(
    ({ set, snapshot }) =>
      (e: React.ChangeEvent<HTMLInputElement>) => {
        set(workspaceMetadataSelector, (state) => ({
          ...state,
          name: e.target.value,
        }));
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        if (metadata.location.kind !== "local" || !metadata.location.saved) {
          saveLocally();
        }
      }
  );
  const onKey = useCallback((e: React.KeyboardEvent) => {
    if (e.key === "Enter" || e.key === "Escape") {
      setEditing(false);
    }
  }, []);
  if (editing) {
    return (
      <InputBox
        type="text"
        value={workspaceMetadata.name}
        autoFocus={true}
        onFocus={(e) => e.target.select()}
        onBlur={() => setEditing(false)}
        onKeyDown={onKey}
        onChange={onChange}
      />
    );
  }
  return (
    <TitleBox onClick={() => setEditing(true)}>
      {workspaceMetadata.name}
    </TitleBox>
  );
}

export default function TopBar() {
  const compileDiagram = useCompileDiagram();
  const resampleDiagram = useResampleDiagram();
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const settings = useRecoilValue(settingsState);
  const saveLocally = useSaveLocally();
  const publishGist = usePublishGist();

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
      {workspaceMetadata.location.kind === "roger" ? (
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
          {workspaceMetadata.location.kind === "gist" && (
            <a
              style={{ textDecoration: "none", color: "inherit" }}
              href={`https://github.com/${workspaceMetadata.location.author}`}
            >
              <AuthorBox>
                <div
                  style={{
                    width: "25px",
                    height: "25px",
                    margin: "5px",
                    backgroundImage: `url(${workspaceMetadata.location.avatar})`,
                    borderRadius: "50%",
                    backgroundSize: "cover",
                    display: "inline-block",
                  }}
                />{" "}
                {workspaceMetadata.location.author}
              </AuthorBox>
            </a>
          )}
          {workspaceMetadata.location.kind === "local" &&
            !workspaceMetadata.location.saved && (
              <BlueButton onClick={saveLocally}>save</BlueButton>
            )}
          {workspaceMetadata.location.kind === "local" &&
            settings.github !== null && (
              <BlueButton onClick={publishGist}>share</BlueButton>
            )}
        </div>
      )}
      <div>
        <BlueButton onClick={compileDiagram}>compile ▶</BlueButton>
        <BlueButton onClick={resampleDiagram}>resample</BlueButton>
      </div>
    </nav>
  );
}
