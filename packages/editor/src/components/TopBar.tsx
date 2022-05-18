import { useCallback, useState } from "react";
import { useRecoilCallback, useRecoilValue } from "recoil";
import styled from "styled-components";
import {
  diagramMetadataSelector,
  workspaceMetadataSelector,
} from "../state/atoms";
import { useCompileDiagram, useSaveLocally } from "../state/callbacks";
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
  const onChange = useRecoilCallback(
    ({ set }) => (e: React.ChangeEvent<HTMLInputElement>) => {
      set(workspaceMetadataSelector, (state) => ({
        ...state,
        name: e.target.value,
      }));
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
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const diagramMetadata = useRecoilValue(diagramMetadataSelector);
  const saveLocally = useSaveLocally();
  const toggleAutostep = useRecoilCallback(({ set }) => () => {
    set(diagramMetadataSelector, (metadata) => ({
      ...metadata,
      autostep: !metadata.autostep,
    }));
  });

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
      <div>
        <EditableTitle />
      </div>
      <div>
        <BlueButton onClick={compileDiagram}>compile ▶</BlueButton>
        {(workspaceMetadata.location.kind === "gist" ||
          !workspaceMetadata.location.saved) && (
          <BlueButton onClick={saveLocally}>save locally</BlueButton>
        )}
        <BlueButton onClick={toggleAutostep}>
          autostep ({diagramMetadata.autostep ? "on" : "off"})
        </BlueButton>
      </div>
    </nav>
  );
}
