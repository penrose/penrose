import { Dispatcher } from "../reducer";
import MonacoEditor, { useMonaco, Monaco } from "@monaco-editor/react";
import { editor } from "monaco-editor";
import { useEffect } from "react";

const monacoOptions: editor.IEditorConstructionOptions = {
  automaticLayout: true,
  minimap: { enabled: false },
  wordWrap: "on",
  tabCompletion: "on",
  fontSize: 16,
  copyWithSyntaxHighlighting: true,
  glyphMargin: false,
};

const abbrevMap = {
  domain: "dsl",
  substance: "sub",
  style: "sty",
};
export default function EditorPane({
  value,
  dispatch,
  languageType,
  setupMonaco,
}: {
  value: string;
  dispatch: Dispatcher;
  languageType: keyof typeof abbrevMap;
  setupMonaco(monaco: Monaco): () => void;
}) {
  const monaco = useMonaco();
  useEffect(() => {
    if (monaco) {
      const dispose = setupMonaco(monaco);
      return dispose;
    }
  }, [monaco, setupMonaco]);
  return (
    <MonacoEditor
      width="100%"
      value={value}
      onChange={(content) =>
        dispatch({
          kind: "CHANGE_CODE",
          lang: abbrevMap[languageType] as any,
          content: content as string,
        })
      }
      defaultLanguage={languageType}
      options={monacoOptions}
    />
  );
}
