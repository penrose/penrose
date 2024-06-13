import { autocompletion } from "@codemirror/autocomplete";
import { EditorView } from "@codemirror/view";
import { DomainEnv } from "@penrose/core/dist/types/domain";
import CodeMirror from "@uiw/react-codemirror";
import { useRef } from "react";
import DomainAutocomplete from "./hooks/domain/domainAutocomplete";
import SubstanceAutocomplete from "./hooks/substance/substanceAutocomplete";
import { domainLanguageSupport } from "./parser/domain/domainLanguage";
import { substanceLanguageSupport } from "./parser/substance/substanceLanguage";

export default function EditorPane({
  value,
  onChange,
  vimMode,
  languageType,
  domainCache,
  readOnly,
}: {
  value: string;
  vimMode: boolean;
  onChange(value: string): void;
  languageType: "substance" | "style" | "domain";
  domainCache: DomainEnv | null;
  readOnly?: boolean;
}) {
  // no idea what this does
  const statusBarRef = useRef<HTMLDivElement>(null);

  // Setup extensions
  const domainCompletionFn = DomainAutocomplete();
  const domainExtensions = [
    autocompletion({ override: [domainCompletionFn] }),
    domainLanguageSupport(),
    EditorView.lineWrapping,
  ];

  const substanceCompletionFn = SubstanceAutocomplete();
  const substanceExtensions = [
    autocompletion({ override: [substanceCompletionFn] }),
    substanceLanguageSupport(),
    EditorView.lineWrapping,
  ];

  const styleExtensions = [
    autocompletion({ override: [substanceCompletionFn] }),
    substanceLanguageSupport(),
    EditorView.lineWrapping,
  ];

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
      />
      <div
        ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
