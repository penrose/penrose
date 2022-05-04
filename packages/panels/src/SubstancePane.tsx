import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { IRange } from "monaco-editor";
import * as React from "react";
import { useCallback, useEffect } from "react";
import {
  SubstanceCompletions,
  SubstanceConfig,
  SubstanceLanguageTokens,
} from "./languages/SubstanceConfig";
import { Dispatcher } from "./reducer";
import { monacoOptions } from "./Util";

const SubstancePane = ({
  value,
  domainCache,
  dispatch,
  numOpen,
}: {
  value: string;
  domainCache: any;
  dispatch: Dispatcher;
  numOpen: number;
}) => {
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
      return { suggestions: SubstanceCompletions(range, domainCache) };
    },
    [domainCache]
  );
  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: "substance" });
      monaco.languages.setLanguageConfiguration("substance", SubstanceConfig);
      if (domainCache) {
        monaco.languages.setMonarchTokensProvider(
          "substance",
          SubstanceLanguageTokens(domainCache)
        );
        const dispose = monaco.languages.registerCompletionItemProvider(
          "substance",
          {
            provideCompletionItems: provideCompletion,
          }
        );
        return () => {
          // prevents duplicates
          dispose.dispose();
        };
      }
    }
  }, [monaco, provideCompletion, domainCache]);
  return (
    <MonacoEditor
      value={value}
      width={`${window.innerWidth / numOpen}px`}
      onChange={(content) =>
        dispatch({
          kind: "CHANGE_CODE",
          lang: "sub",
          content: content as string,
        })
      }
      defaultLanguage="substance"
      options={monacoOptions}
    />
  );
};

export default SubstancePane;
