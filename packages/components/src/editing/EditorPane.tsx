import MonacoEditor, { Monaco, useMonaco } from "@monaco-editor/react";
import { editor } from "monaco-editor";
import { useEffect, useRef } from "react";

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
  onChange,
  vimMode,
  languageType,
  setupMonaco,
}: {
  value: string;
  vimMode: boolean;
  onChange(value: string): void;
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
  const onEditorMount = (editorArg: editor.IStandaloneCodeEditor) => {
    editorRef.current = editorArg;
  };

  return (
    <div style={{ width: "100%", height: "100%", position: "relative" }}>
      <MonacoEditor
        width="100%"
        value={value}
        onChange={(v) => onChange(v ?? "")}
        defaultLanguage={languageType}
        // HACK
        options={monacoOptions as any}
        onMount={onEditorMount as any}
      />
      <div
        // ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
