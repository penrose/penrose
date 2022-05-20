import localforage from "localforage";
import { useEffect } from "react";
import Select from "react-select";
import { useRecoilCallback, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import { RogerState } from "../App";
import {
  currentWorkspaceState,
  ProgramType,
  WorkspaceMetadata,
} from "../state/atoms";
export default function RogerPanel({
  rogerState,
  ws,
}: {
  rogerState: RogerState;
  ws: WebSocket | null;
}) {
  const workspace = useRecoilValue(currentWorkspaceState);
  const { substance, style, domain } = workspace.files;
  const onSelection = useRecoilCallback(
    ({ set }) => (val: string, key: ProgramType) => {
      set(currentWorkspaceState, (state) => {
        const files = {
          ...state.files,
          [key]: { ...state.files[key], name: val },
        };
        localforage.setItem("selected_roger_files", {
          substance: files.substance.name,
          style: files.style.name,
          domain: files.domain.name,
        });
        return {
          ...state,
          metadata: {
            ...state.metadata,
            location: { kind: "roger" },
            id: uuid(),
          } as WorkspaceMetadata,
          files,
        };
      });
      if (ws !== null) {
        ws.send(
          JSON.stringify({
            kind: "retrieve_file",
            fileName: val,
          })
        );
      }
    }
  );
  useEffect(() => {
    if (rogerState.kind === "connected") {
      (async () => {
        const selectedFiles = (await localforage.getItem(
          "selected_roger_files"
        )) as any;
        if (selectedFiles !== null) {
          onSelection(selectedFiles.domain, "domain");
          onSelection(selectedFiles.style, "style");
          onSelection(selectedFiles.substance, "substance");
        }
      })();
    }
  }, [rogerState.kind]);
  if (rogerState.kind === "disconnected") {
    return <h1>disconnected</h1>;
  }
  return (
    <div>
      <h2>substance</h2>
      <Select
        options={rogerState.substance.map((val) => ({ val }))}
        getOptionLabel={({ val }) => val}
        getOptionValue={({ val }) => val}
        onChange={(e) => onSelection(e?.val ?? "", "substance")}
        value={{ val: substance.name }}
      />
      <h2>style</h2>
      <Select
        options={rogerState.style.map((val) => ({ val }))}
        getOptionLabel={({ val }) => val}
        getOptionValue={({ val }) => val}
        onChange={(e) => onSelection(e?.val ?? "", "style")}
        value={{ val: style.name }}
      />
      <h2>domain</h2>
      <Select
        options={rogerState.domain.map((val) => ({ val }))}
        getOptionLabel={({ val }) => val}
        getOptionValue={({ val }) => val}
        onChange={(e) => onSelection(e?.val ?? "", "domain")}
        value={{ val: domain.name }}
      />
    </div>
  );
}
