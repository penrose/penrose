import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { compileDomain } from "@penrose/core";
import { editor, IRange } from "monaco-editor";
import { useCallback, useEffect } from "react";
import {
  SubstanceCompletions,
  SubstanceConfig,
  SubstanceLanguageTokens,
} from "./editing/languages/SubstanceConfig";

export const monacoOptions: editor.IStandaloneEditorConstructionOptions = {
  automaticLayout: true,
  readOnly: true,
  minimap: { enabled: false },
  wordWrap: "on",
  lineNumbers: "off",
  fontSize: 16,
  scrollbar: {
    handleMouseWheel: false,
  },
  scrollBeyondLastLine: false,
  renderLineHighlight: "none",
};

const Listing = ({
  domain,
  substance,
  width,
  height,
}: {
  domain: string;
  substance: string;
  width: string;
  height: string;
}) => {
  const env = compileDomain(domain).unsafelyUnwrap();
  const monaco = useMonaco();
  const provideCompletion = useCallback(
    (model, position) => {
      const word = model.getWordUntilPosition(position);
      const range: IRange = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };
      return { suggestions: SubstanceCompletions(range, env) };
    },
    [env]
  );
  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: "substance" });
      monaco.languages.setLanguageConfiguration("substance", SubstanceConfig);
      if (env) {
        monaco.languages.setMonarchTokensProvider(
          "substance",
          SubstanceLanguageTokens(env)
        );
        const dispose = monaco.languages.registerCompletionItemProvider(
          "substance",
          {
            // HACK:
            provideCompletionItems: provideCompletion as any,
          }
        );
        return () => {
          // prevents duplicates
          dispose.dispose();
        };
      }
    }
  }, [monaco, provideCompletion, env]);
  return (
    <MonacoEditor
      value={substance}
      width={width}
      height={height}
      defaultLanguage="substance"
      options={monacoOptions}
    />
  );
};

export default Listing;
