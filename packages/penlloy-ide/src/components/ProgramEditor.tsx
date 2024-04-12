import { EditorPane } from "@penrose/components";
import { useEffect } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import {
  ProgramType,
  currentDiagramState,
  currentDirtyProgramSelector,
  currentDomainCacheSelector,
  currentProgramSelector,
} from "../state/atoms";

/**
 * Create a new editor for a given program type. Substance and domain editors are read-only. Substance and domain editors also automatically commit dirty contents.
 * @param programType
 * @returns a tuple [editor, commitDirty], where editor is the JSX element of the editor and commitDirty is a
 * function that commits the dirty program content to the current program content
 */
export const makeEditor = (
  programType: ProgramType,
): [JSX.Element, () => void] => {
  const [dirtyProgramContent, setDirtyProgramContent] = useRecoilState(
    currentDirtyProgramSelector(programType),
  );

  const [programContent, setProgramContent] = useRecoilState(
    currentProgramSelector(programType),
  );

  const domainCache = useRecoilValue(currentDomainCacheSelector);

  const readOnly = programType !== "style";

  const [diagram] = useRecoilState(currentDiagramState);
  const { error, warnings } = diagram;

  const commitDirty = () => setProgramContent(dirtyProgramContent);

  if (programType !== "style") {
    // need to automatically commit dirty program content for non-Style programs
    useEffect(commitDirty, [dirtyProgramContent]);
  }

  return [
    <EditorPane
      value={programContent}
      vimMode={false}
      languageType={programType}
      domainCache={domainCache}
      onChange={setDirtyProgramContent}
      readOnly={false}
      onWrite={() => {}}
      error={error}
      warnings={warnings}
    />,
    commitDirty,
  ];
};
