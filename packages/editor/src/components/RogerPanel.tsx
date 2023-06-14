import localforage from "localforage";
import { useEffect } from "react";
import Select from "react-select";
import { useRecoilCallback, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import {
  ProgramType,
  RogerState,
  currentWorkspaceState,
} from "../state/atoms.js";
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
    ({ set }) =>
      (val: string, key: ProgramType) => {
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
          const { location } = state.metadata;

          const fileLocations =
            location.kind === "roger"
              ? { ...location, [key]: val }
              : {
                  substance: undefined,
                  style: undefined,
                  domain: undefined,
                };

          return {
            ...state,
            metadata: {
              ...state.metadata,
              location: {
                kind: "roger" as const,
                // TODO: only set the root of the location if a style file is selected
                // TODO: do path processing in a more principled way
                ...fileLocations,
              },
              id: uuid(),
            },
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
    return (
      <div>
        <h1>Local development mode</h1>
        <p>
          Run <code>roger watch</code> to start serving your local Penrose trio.
        </p>
      </div>
    );
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
      <h2>trio</h2>
      <Select
        options={rogerState.trio.map((val) => ({ val }))}
        getOptionLabel={({ val }) => val}
        getOptionValue={({ val }) => val}
        onChange={(e) => {
          ws?.addEventListener("message", (e) => {
            const parsed = JSON.parse(e.data);
            if (parsed.kind !== "trio_file") return;
            const key: "substance" | "style" | "domain" = parsed.type;
            const val = parsed.fileName;
            onSelection(val, key);
          });
          ws?.send(
            JSON.stringify({
              kind: "retrieve_trio",
              path: e?.val,
              token: uuid(),
            })
          );
        }}
        value={{ val: ".trio.json" }}
      />
    </div>
  );
}
