import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { Env, errLocs, showError } from "@penrose/core";
import {
  DomainError,
  RuntimeError,
  StyleError,
  StyleErrorList,
  StyleWarning,
  SubstanceError,
} from "@penrose/core/dist/types/errors.js";
import { ErrorLoc } from "@penrose/core/dist/utils/Util.js";
import { MarkerSeverity, editor } from "monaco-editor";
import { VimMode, initVimMode } from "monaco-vim";
import { useEffect, useRef } from "react";
import { SetupDomainMonaco } from "./languages/DomainConfig.js";
import { SetupStyleMonaco } from "./languages/StyleConfig.js";
import { SetupSubstanceMonaco } from "./languages/SubstanceConfig.js";

const monacoOptions = (
  vimMode: boolean,
): editor.IEditorConstructionOptions => ({
  automaticLayout: true,
  minimap: { enabled: false },
  wordWrap: "on",
  tabCompletion: "on",
  fontSize: 16,
  copyWithSyntaxHighlighting: true,
  glyphMargin: false,
  cursorStyle: vimMode ? "block" : "line",
  fixedOverflowWidgets: true,
});

type IndividualError = Exclude<
  StyleError | DomainError | SubstanceError | RuntimeError | StyleWarning,
  StyleErrorList
>;

type ErrLocPair = {
  err: IndividualError;
  loc: ErrorLoc;
};

const errLocPairs = (
  err: StyleError | DomainError | SubstanceError | RuntimeError | StyleWarning,
): ErrLocPair[] => {
  if (err.tag === "StyleErrorList") {
    const pairs = err.errors.map(errLocPairs);
    return pairs.flat(1);
  } else {
    const locs = errLocs(err);
    return locs.length > 0 ? [{ err, loc: locs[0] }] : [];
  }
};

export default function EditorPane({
  value,
  onChange,
  vimMode,
  languageType,
  domainCache,
  readOnly,
  onWrite,
  error,
  warnings,
}: {
  value: string;
  vimMode: boolean;
  onChange(value: string): void;
  languageType: "substance" | "style" | "domain";
  domainCache: Env | null;
  readOnly?: boolean;
  /// In vim mode, this is called when the user calls :w
  onWrite?: () => void;
  error: StyleError | DomainError | SubstanceError | RuntimeError | null;
  warnings: StyleWarning[];
}) {
  const monaco = useMonaco();
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
  const statusBarRef = useRef<HTMLDivElement>(null);

  if (monaco !== null && onWrite !== undefined) {
    editorRef.current?.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
      onWrite,
    );
  }

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
    if (onWrite && !VimMode.Vim.SET_WRITE) {
      // HACK to prevent multiple definitions of :w
      VimMode.Vim.SET_WRITE = true;
      VimMode.Vim.defineEx("write", "w", onWrite);
    }
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

  useEffect(() => {
    const errPairs = error === null ? [] : errLocPairs(error);
    const warningPairs = warnings.map(errLocPairs).flat(1);
    const errMarkers: editor.IMarkerData[] = errPairs
      .filter(({ loc }) => loc.type.toLowerCase() === languageType)
      .map(({ loc, err }) => ({
        startLineNumber: loc.range.start.line,
        startColumn: loc.range.start.col + 1,
        endLineNumber: loc.range.end.line,
        endColumn: loc.range.end.col + 1,
        message: showError(err),
        severity: MarkerSeverity.Error,
      }));
    const warningMarkers: editor.IMarkerData[] = warningPairs
      .filter(({ loc }) => loc.type.toLowerCase() === languageType)
      .map(({ loc, err: warn }) => ({
        startLineNumber: loc.range.start.line,
        startColumn: loc.range.start.col + 1,
        endLineNumber: loc.range.end.line,
        endColumn: loc.range.end.col + 1,
        message: showError(warn),
        severity: MarkerSeverity.Warning,
      }));

    const markers = [...errMarkers, ...warningMarkers];
    if (monaco && editorRef.current) {
      monaco.editor.setModelMarkers(editorRef.current.getModel()!, "", markers);
    }
  }, [monaco, editorRef.current, languageType, error, warnings, value]);

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
