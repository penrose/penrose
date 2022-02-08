import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { IRange } from "monaco-editor";
import * as React from "react";
import { useEffect } from "react";
import {
  DomainCompletions,
  DomainConfig,
  DomainLanguageTokens,
} from "./languages/DomainConfig";
import { Dispatcher } from "./reducer";
import { monacoOptions } from "./Util";

const DomainPane = ({
  value,
  dispatch,
  numOpen,
}: {
  value: string;
  dispatch: Dispatcher;
  numOpen: number;
}) => {
  const monaco = useMonaco();
  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: "domain" });
      monaco.languages.setLanguageConfiguration("domain", DomainConfig);
      monaco.languages.setMonarchTokensProvider(
        "domain",
        DomainLanguageTokens()
      );
      const dispose = monaco.languages.registerCompletionItemProvider(
        "domain",
        {
          provideCompletionItems: (model, position) => {
            const word = model.getWordUntilPosition(position);
            const range: IRange = {
              startLineNumber: position.lineNumber,
              endLineNumber: position.lineNumber,
              startColumn: word.startColumn,
              endColumn: word.endColumn,
            };
            return { suggestions: DomainCompletions(range) };
          },
        }
      );
      return () => {
        dispose.dispose();
      };
    }
  }, [monaco]);
  return (
    <MonacoEditor
      value={value}
      width={`${window.innerWidth / numOpen}px`}
      onChange={(content) =>
        dispatch({
          kind: "CHANGE_CODE",
          lang: "dsl",
          content: content as string,
        })
      }
      defaultLanguage="domain"
      options={monacoOptions}
    />
  );
};

export default DomainPane;
