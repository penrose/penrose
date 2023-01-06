import { EditorPane } from "@penrose/components";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import {
  domainCacheState,
  fileContentsSelector,
  ProgramType,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms";
import { useCompileDiagram } from "../state/callbacks";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind)
  );
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const domainCache = useRecoilValue(domainCacheState);
  const compileDiagram = useCompileDiagram();
  const settings = useRecoilValueLoadable(settingsState);
  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <EditorPane
      value={programState.contents}
      vimMode={settings.contents.vimMode}
      languageType={kind}
      domainCache={domainCache}
      onChange={(v: string) =>
        setProgramState((state) => ({ ...state, contents: v }))
      }
      readOnly={workspaceMetadata.location.kind === "roger"}
      onWrite={compileDiagram}
    />
  );
}
