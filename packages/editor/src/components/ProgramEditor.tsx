import { EditorPane } from "@penrose/components";
import { useCallback } from "react";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import {
  ProgramType,
  diagramState,
  domainCacheState,
  fileContentsSelector,
  settingsState,
  substanceCacheState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { useCompileDiagram } from "../state/callbacks.js";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind),
  );
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const domainCache = useRecoilValue(domainCacheState);
  const substanceCache = useRecoilValue(substanceCacheState);
  const compileDiagram = useCompileDiagram();
  const settings = useRecoilValueLoadable(settingsState);
  const [diagram] = useRecoilState(diagramState);
  const { error, warnings } = diagram;
  const onChange = useCallback(
    (v: string) => {
      setProgramState((state) => ({ ...state, contents: v }));
    },
    [setProgramState],
  );
  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <EditorPane
      value={programState.contents}
      vimMode={settings.contents.vimMode}
      onChange={onChange}
      languageType={kind}
      domainCache={domainCache}
      substanceCache={substanceCache}
      readOnly={workspaceMetadata.location.kind === "roger"}
    />
  );
}
