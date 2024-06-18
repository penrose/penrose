import { autocompletion } from "@codemirror/autocomplete";
import { lintGutter } from "@codemirror/lint";
import { EditorView } from "@codemirror/view";
import {
  DomainError,
  RuntimeError,
  StyleError,
  StyleWarning,
  SubstanceError,
} from "@penrose/core/dist/types/errors.js";
import CodeMirror from "@uiw/react-codemirror";
import { useRef } from "react";
import { DomainCache, SubstanceCache } from "../editing/types";
import DomainAutocomplete from "./hooks/domain/domainAutocomplete";
import StyleAutocomplete from "./hooks/style/styleAutocomplete";
import SubstanceAutocomplete from "./hooks/substance/substanceAutocomplete";
import { createLinter } from "./hooks/useLinter";
import { domainLanguageSupport } from "./parser/domain/domainLanguage";
import { styleLanguageSupport } from "./parser/style/styleLanguage";
import { substanceLanguageSupport } from "./parser/substance/substanceLanguage";
import { penroseEditorTheme } from "./theme";
// import { ErrorLoc } from "@penrose/core/dist/utils/Util.js";
// import { errLocs, showError } from "@penrose/core";
import { linter } from "@codemirror/lint";

export default function EditorPane({
  value,
  onChange,
  vimMode,
  languageType,
  domainCache,
  substanceCache,
  readOnly,
  error,
  warnings,
  showCompileErrs,
}: {
  value: string;
  vimMode: boolean;
  onChange(value: string): void;
  languageType: "substance" | "style" | "domain";
  domainCache: DomainCache;
  substanceCache: SubstanceCache;
  readOnly?: boolean;
  error: StyleError | DomainError | SubstanceError | RuntimeError | null;
  warnings: StyleWarning[];
  showCompileErrs: boolean;
}) {
  // console.log("err", error);
  // console.log("warn", warnings);
  // no idea what this does
  const statusBarRef = useRef<HTMLDivElement>(null);

  const SetFontSize = EditorView.theme({
    "&": {
      fontSize: "12pt",
    },
  });
  // console.log(languageType);
  const lintObject = linter(
    createLinter(error, warnings, languageType, showCompileErrs),
  );

  const defaultExtensions = [
    EditorView.lineWrapping,
    SetFontSize,
    lintObject,
    lintGutter(),
  ];

  let domainCompletionFn = DomainAutocomplete(domainCache);

  const domainExtensions = [
    autocompletion({ override: [domainCompletionFn] }),
    domainLanguageSupport(),
  ].concat(defaultExtensions);

  const substanceCompletionFn = SubstanceAutocomplete(
    domainCache,
    substanceCache,
  );
  const substanceExtensions = [
    autocompletion({ override: [substanceCompletionFn] }),
    substanceLanguageSupport(),
  ].concat(defaultExtensions);

  const styleCompletionFn = StyleAutocomplete();
  const styleExtensions = [
    autocompletion({ override: [styleCompletionFn] }),
    styleLanguageSupport(),
  ].concat(defaultExtensions);
  // const styleExtensions = defaultExtensions.concat(styleLanguageSupport());

  let extensionsList =
    languageType === "domain"
      ? domainExtensions
      : languageType === "substance"
      ? substanceExtensions
      : styleExtensions;

  return (
    <div style={{ width: "100%", height: "100%", position: "relative" }}>
      <CodeMirror
        value={value}
        extensions={extensionsList}
        onChange={onChange}
        theme={penroseEditorTheme}
      />
      <div
        ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
