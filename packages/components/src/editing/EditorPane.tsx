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
import { useEffect, useRef, useState } from "react";
import {
  DomainCache,
  ShapeDefinitions,
  SubstanceCache,
} from "../editing/types";
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

import {
  ShapeType,
  makeCanvas,
  sampleShape,
  shapeTypes,
  simpleContext,
} from "@penrose/core";

/**
 * Retrieves defintions for all shapes and writes their properties to a
 * hashmap object with shapeName as the top level key with the value being
 * a hashmap of properties that belong to the shape as keys with type as value
 */
const getShapeDefs = (): ShapeDefinitions => {
  const shapeProps = {} as ShapeDefinitions;
  const size = 311; // placeholder, this doesn't matter

  for (const shapeName of shapeTypes) {
    const shapeSample = sampleShape(
      shapeName as ShapeType,
      simpleContext("ShapeProps dummy"),
      makeCanvas(size, size),
    );

    shapeProps[shapeName] = Object.fromEntries(
      Object.entries(shapeSample).map(([key, value]) => [key, value.tag]),
    );
  }

  return shapeProps;
};

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

  const [shapeDefs, setshapeDefs] = useState<ShapeDefinitions>({});

  useEffect(() => {
    setshapeDefs(getShapeDefs());
  }, []);

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

  const styleCompletionFn = StyleAutocomplete(domainCache, shapeDefs);
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
