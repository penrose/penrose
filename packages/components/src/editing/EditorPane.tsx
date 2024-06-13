import { autocompletion } from "@codemirror/autocomplete";
import { EditorView } from "@codemirror/view";
import CodeMirror from "@uiw/react-codemirror";
import { useRef } from "react";
import { DomainCache } from "../editing/types";
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
  domainCache: DomainCache;
  readOnly?: boolean;
}) {
  // no idea what this does
  const statusBarRef = useRef<HTMLDivElement>(null);

  const SetFontSize = EditorView.theme({
    "&": {
      fontSize: "12pt",
    },
  });

  const defaultExtensions = [EditorView.lineWrapping, SetFontSize];
  // console.log(domainCache);
  // Setup extensions
  const domainCompletionFn = DomainAutocomplete(domainCache);

  const domainExtensions = [
    autocompletion({ override: [domainCompletionFn] }),
    domainLanguageSupport(),
  ].concat(defaultExtensions);

  const substanceCompletionFn = SubstanceAutocomplete();
  const substanceExtensions = [
    autocompletion({ override: [substanceCompletionFn] }),
    substanceLanguageSupport(),
  ].concat(defaultExtensions);

  const styleExtensions = [
    autocompletion({ override: [substanceCompletionFn] }),
    substanceLanguageSupport(),
    EditorView.lineWrapping,
  ].concat(defaultExtensions);

  let extensionsList =
    languageType === "domain"
      ? domainExtensions
      : languageType === "substance"
      ? substanceExtensions
      : styleExtensions;

  // console.log(domainCache);

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
