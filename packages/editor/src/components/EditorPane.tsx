import { Dispatcher } from "../reducer";
import MonacoEditor, { useMonaco, Monaco } from "@monaco-editor/react";
import { initVimMode } from "monaco-vim";
import { editor } from "monaco-editor";
import { useCallback, useEffect, useRef } from "react";

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
  vimMode,
  languageType,
  setupMonaco,
}: {
  value: string;
  vimMode: boolean;
  dispatch: Dispatcher;
  languageType: keyof typeof abbrevMap;
  setupMonaco(monaco: Monaco): () => void;
}) {
  const monaco = useMonaco();
  const statusBarRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (monaco) {
      const dispose = setupMonaco(monaco);
      return () => {
        dispose();
      };
    }
  }, [monaco, setupMonaco]);
  // if (vimMode && statusBarRef.current) {
  // const vimModeInstance = initVimMode(editor, statusBarRef.current);
  // return () => {
  // vimModeInstance.dispose();
  // };
  return (
    <div style={{ width: "100%", height: "100%" }}>
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
      <div ref={statusBarRef} />
    </div>
  );
}
