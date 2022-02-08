import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { IRange } from "monaco-editor";
import * as React from "react";
import { useEffect } from "react";
import {
  StyleCompletions,
  StyleConfig,
  StyleLanguageTokens,
} from "./languages/StyleConfig";
import { Dispatcher } from "./reducer";
import { monacoOptions } from "./Util";

const StylePane = ({
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
      monaco.languages.register({ id: "style" });
      monaco.languages.setLanguageConfiguration("style", StyleConfig);
      monaco.languages.setMonarchTokensProvider("style", StyleLanguageTokens);
      const dispose = monaco.languages.registerCompletionItemProvider("style", {
        provideCompletionItems: (model, position) => {
          const word = model.getWordUntilPosition(position);
          const range: IRange = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: word.startColumn,
            endColumn: word.endColumn,
          };
          return { suggestions: StyleCompletions(range) };
        },
      });
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
          lang: "sty",
          content: content as string,
        })
      }
      defaultLanguage="style"
      options={monacoOptions}
    />
  );
};

export default StylePane;
