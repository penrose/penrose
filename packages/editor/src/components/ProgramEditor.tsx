import { EditorPane } from "@penrose/components";
import { useRecoilState, useRecoilValue } from "recoil";
import {
  domainCacheState,
  domainFileState,
  styleFileState,
  substanceFileState,
} from "../state/atoms";
export default function ProgramEditor({
  kind,
}: {
  kind: "substance" | "style" | "domain";
}) {
  // TODO: if substance, selector the domainCache. Don't need a state i doint think. Derived state!
  const [programState, setProgramState] = useRecoilState(
    kind === "domain"
      ? domainFileState
      : kind === "style"
      ? styleFileState
      : substanceFileState
  );
  const domainCache = useRecoilValue(domainCacheState);
  return (
    <EditorPane
      value={programState.contents}
      vimMode={false}
      languageType={kind}
      domainCache={domainCache}
      onChange={(v) => setProgramState((state) => ({ ...state, contents: v }))}
    />
  );
}
