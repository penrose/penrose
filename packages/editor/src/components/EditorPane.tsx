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
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
  const statusBarRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (monaco) {
      const dispose = setupMonaco(monaco);
      return () => {
        dispose();
      };
    }
  }, [monaco, setupMonaco, vimMode]);
  useEffect(() => {
    if (vimMode && statusBarRef.current && editorRef.current) {
      const vimModeInstance = initVimMode(
        editorRef.current,
        statusBarRef.current
      );
      return () => vimModeInstance.dispose();
    }
  }, [vimMode, editorRef.current, statusBarRef.current]);
  const onEditorMount = (editorArg: editor.IStandaloneCodeEditor) => {
    editorRef.current = editorArg;
  };

  return (
    <div style={{ width: "100%", height: "100%", position: "relative" }}>
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
        onMount={onEditorMount}
      />
      <div
        ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
