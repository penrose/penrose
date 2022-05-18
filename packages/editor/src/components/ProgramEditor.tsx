import { EditorPane } from "@penrose/components";
import { useRecoilState, useRecoilValue } from "recoil";
import {
  domainCacheState,
  fileContentsSelector,
  ProgramType,
} from "../state/atoms";
export default function ProgramEditor({ kind }: { kind: ProgramType }) {
  // TODO: if substance, selector the domainCache. Don't need a state i doint think. Derived state!
  const [programState, setProgramState] = useRecoilState(
    fileContentsSelector(kind)
  );
  const domainCache = useRecoilValue(domainCacheState);
  return (
    <EditorPane
      value={programState.contents}
      vimMode={false}
      languageType={kind}
      domainCache={domainCache}
      onChange={(v: string) =>
        setProgramState((state) => ({ ...state, contents: v }))
      }
    />
  );
}
