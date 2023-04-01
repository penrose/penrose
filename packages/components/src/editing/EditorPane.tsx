import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { Env } from "@penrose/core";
import { editor } from "monaco-editor";
import { initVimMode, VimMode } from "monaco-vim";
import { useEffect, useRef } from "react";
import { SetupDomainMonaco } from "./languages/DomainConfig";
import { SetupStyleMonaco } from "./languages/StyleConfig";
import { SetupSubstanceMonaco } from "./languages/SubstanceConfig";

import {addKeybinds, addVimCommands, PenroseCalls } from "./EditorConfig";

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
  vimMode,
  languageType,
  domainCache,
  readOnly,
  penroseCalls,
}: {
  value: string;
  vimMode: boolean;
  languageType: "substance" | "style" | "domain";
  domainCache: Env | null;
  readOnly?: boolean;
  penroseCalls: PenroseCalls;
}) {
  const monaco = useMonaco();
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
  const statusBarRef = useRef<HTMLDivElement>(null);
  const onWrite = penroseCalls.onWrite;

  addKeybinds(editorRef, penroseCalls);

  // if(monaco && editorRef.current){
  //   addHoverProvider(monaco, editorRef.current);
  // }

  useEffect(() => {
    if (monaco) {
      const dispose =
        languageType === "domain"
          ? SetupDomainMonaco(monaco)
          : languageType === "style"
          ? SetupStyleMonaco(monaco)
          : SetupSubstanceMonaco(domainCache)(monaco);
      return () => {
        dispose();
      };
    }
  }, [monaco, vimMode, languageType, domainCache]);



  useEffect(() => {
    if (monaco) {
      // TODO: change model language
      // monaco.editor.setModelLanguage(monaco.editor.getModel(), "penrose-style")
    }
  }, [languageType])

  useEffect(() => {
    addVimCommands(penroseCalls)

  }, [onWrite]);

  useEffect(() => {
    if (vimMode && editorRef.current) {
      const vim = initVimMode(editorRef.current, statusBarRef.current);
      return () => {
        vim.dispose();
      };
    }
  }, [vimMode, editorRef.current]);

  const onEditorMount = (editorArg: editor.IStandaloneCodeEditor) => {
    editorRef.current = editorArg;
  };

  return (
    <div style={{ width: "100%", height: "100%", position: "relative" }}>
      <MonacoEditor
        width="100%"
        value={value}
        onChange={(v) => penroseCalls.onChange(v ?? "")}
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
