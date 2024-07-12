import { EditorPane } from "@penrose/components";
import { useCallback } from "react";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import {
  ProgramType,
  codemirrorHistory,
  diagramErrorSelector,
  diagramWarningsSelector,
  domainCacheState,
  fileContentsSelector,
  settingsState,
  showCompileErrsState,
  substanceCacheState,
} from "../state/atoms.js";
import { useCompileDiagram } from "../state/callbacks.js";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind),
  );
  const domainCache = useRecoilValue(domainCacheState);
  const substanceCache = useRecoilValue(substanceCacheState);
  const codemirrorHistoryState = useRecoilValue(codemirrorHistory);
  const compileDiagram = useCompileDiagram();
  const settings = useRecoilValueLoadable(settingsState);
  const error = useRecoilValue(diagramErrorSelector);
  const warnings = useRecoilValue(diagramWarningsSelector);

  /*
  When a user clicks compile, if there are compiler errors these are shown.
  On edit, these are hidden and parser errors are shown.
  showCompileErrs is set to true by useCompile and used by EditorPane and the
  linter.
  */
  const [showCompileErrs, setShowCompileErrs] =
    useRecoilState(showCompileErrsState);
  const onChange = useCallback(
    (v: string) => {
      setProgramState((state) => ({ ...state, contents: v }));
      setShowCompileErrs(false);
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
      error={error}
      darkMode={false}
      warnings={warnings}
      showCompileErrs={showCompileErrs}
      codemirrorHistoryState={codemirrorHistoryState}
      readOnly={false}
      onWrite={compileDiagram}
    />
  );
}
