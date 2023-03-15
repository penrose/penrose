import { EditorPane } from "@penrose/components";
import { useCallback } from "react";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import {
  domainCacheState,
  fileContentsSelector,
  ProgramType,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms";
import { useCompileDiagram, useResampleDiagram } from "../state/callbacks";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind)
  );
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const domainCache = useRecoilValue(domainCacheState);
  const compileDiagram = useCompileDiagram();
  const resampleDiagram = useResampleDiagram();
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

  const penroseCalls = {
    onWrite: compileDiagram,
    onChange: onChange,
    resampleDiagram: resampleDiagram,
  };

  return (
    <EditorPane
      value={programState.contents}
      vimMode={settings.contents.vimMode}
      languageType={kind}
      domainCache={domainCache}
      readOnly={workspaceMetadata.location.kind === "roger"}
      penroseCalls={penroseCalls}
    />
  );
}
