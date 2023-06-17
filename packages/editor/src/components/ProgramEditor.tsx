import { EditorPane } from "@penrose/components";
import { useCallback } from "react";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import {
  ProgramType,
  domainCacheState,
  fileContentsSelector,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { useCompileDiagram } from "../state/callbacks.js";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind)
  );
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const domainCache = useRecoilValue(domainCacheState);
  const compileDiagram = useCompileDiagram();
  const settings = useRecoilValueLoadable(settingsState);
  const onChange = useCallback(
    (v: string) => {
      setProgramState((state) => ({ ...state, contents: v }));
    },
    [setProgramState]
  );
  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <EditorPane
      value={programState.contents}
      vimMode={settings.contents.vimMode}
      languageType={kind}
      domainCache={domainCache}
      onChange={onChange}
      readOnly={workspaceMetadata.location.kind === "roger"}
      onWrite={compileDiagram}
    />
  );
}
