import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { Env } from "@penrose/core";
import { editor } from "monaco-editor";
import { initVimMode, VimMode } from "monaco-vim";
import { useEffect, useRef } from "react";
import { SetupDomainMonaco } from "./languages/DomainConfig";
import { SetupStyleMonaco } from "./languages/StyleConfig";
import { SetupSubstanceMonaco } from "./languages/SubstanceConfig";

const monacoOptions = (
  vimMode: boolean
): editor.IEditorConstructionOptions => ({
  automaticLayout: true,
  minimap: { enabled: false },
  wordWrap: "on",
  tabCompletion: "on",
  fontSize: 16,
  copyWithSyntaxHighlighting: true,
  glyphMargin: false,
  cursorStyle: vimMode ? "block" : "line",
});

export default function EditorPane({
  value,
  onChange,
  vimMode,
  languageType,
  domainCache,
  readOnly,
  onWrite,
}: {
  value: string;
  vimMode: boolean;
  onChange(value: string): void;
  languageType: "substance" | "style" | "domain";
  domainCache: Env | null;
  readOnly?: boolean;
  /// In vim mode, this is called when the user calls :w
  onWrite?: () => void;
}) {
  const monaco = useMonaco();
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
  const statusBarRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (monaco) {
      let vim: null | { dispose(): void } = null;
      const dispose =
        languageType === "domain"
          ? SetupDomainMonaco(monaco)
          : languageType === "style"
          ? SetupStyleMonaco(monaco)
          : SetupSubstanceMonaco(domainCache)(monaco);
      if (vimMode && editorRef.current) {
        if (onWrite) {
          VimMode.Vim.defineEx("write", "w", onWrite);
        }
        vim = initVimMode(editorRef.current, statusBarRef.current);
      }
      return () => {
        dispose();
        if (vim !== null) {
          vim.dispose();
        }
      };
    }
  }, [monaco, vimMode, languageType, domainCache, editorRef.current, onWrite]);
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
        options={{ ...(monacoOptions(vimMode) as any), readOnly }}
        onMount={onEditorMount as any}
      />
      <div
        ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
