import { autocompletion, completionKeymap } from "@codemirror/autocomplete";

import { lintGutter, linter } from "@codemirror/lint";
import { Prec } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import {
  DomainError,
  RuntimeError,
  StyleError,
  StyleWarning,
  SubstanceError,
} from "@penrose/core/dist/types/errors.js";
import { Vim, vim } from "@replit/codemirror-vim";
import { color } from "@uiw/codemirror-extensions-color";
import CodeMirror from "@uiw/react-codemirror";
import { useEffect, useRef, useState } from "react";
import {
  DomainCache,
  ShapeDefinitions,
  SubstanceCache,
} from "../editing/types";
import DomainAutocomplete from "./hooks/domain/domainAutocomplete";
import { getShapeDefs } from "./hooks/hooksUtils";
import StyleAutocomplete from "./hooks/style/styleAutocomplete";
import { wordHover } from "./hooks/style/styleTooltips";
import SubstanceAutocomplete from "./hooks/substance/substanceAutocomplete";
import { createLinter } from "./hooks/useLinter";
import { domainLanguageSupport } from "./parser/domain/domainLanguage";
import { styleLanguageSupport } from "./parser/style/styleLanguage";
import { substanceLanguageSupport } from "./parser/substance/substanceLanguage";
import { penroseTheme } from "./theme";

export default function EditorPane({
  value,
  onChange,
  vimMode,
  languageType,
  domainCache,
  substanceCache,
  error,
  warnings,
  showCompileErrs,
  codemirrorHistoryState,
  readOnly,
  darkMode,
  height,
  minHeight,
  maxHeight,
  width,
  minWidth,
  maxWidth,
  onWrite,
}: {
  value: string;
  height?: string;
  minHeight?: string;
  maxHeight?: string;
  width?: string;
  minWidth?: string;
  maxWidth?: string;
  vimMode: boolean;
  onChange(value: string): void;
  languageType: "substance" | "style" | "domain";
  domainCache: DomainCache;
  substanceCache: SubstanceCache;
  error: StyleError | DomainError | SubstanceError | RuntimeError | null;
  warnings: StyleWarning[];
  showCompileErrs: boolean;
  readOnly: boolean;
  darkMode: boolean;
  codemirrorHistoryState: boolean;
  onWrite?: () => void;
}) {
  const statusBarRef = useRef<HTMLDivElement>(null);

  const ResponsiveStyles = EditorView.theme({
    /*
     * Hide autocomplete info box on mobile devices
     * https://github.com/codemirror/autocomplete/blob/82893f890f37dc182e0dd1e585a62a35e8819cfc/src/theme.ts#L73
     * Seems like on mobile, the info boxes automatically swap to the "narrow"
     * class names
     */
    "@media screen and (max-width: 800px)": {
      ".cm-completionInfo-right, .cm-completionInfo-left, .cm-completionInfo.cm-completionInfo-left-narrow, .cm-completionInfo.cm-completionInfo-right-narrow":
        {
          display: "none",
        },
    },
  });

  const lintObject = linter(
    createLinter(error, warnings, languageType, showCompileErrs),
  );

  const defaultExtensions = [
    EditorView.lineWrapping,
    ResponsiveStyles,
    lintObject,
    keymap.of(completionKeymap),
    lintGutter(),
    color,
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
    wordHover,
  ].concat(defaultExtensions);

  let extensionsList =
    languageType === "domain"
      ? domainExtensions
      : languageType === "substance"
      ? substanceExtensions
      : styleExtensions;

  // onWrite may be undefined (like in Listing.tsx)
  if (onWrite) {
    // Keymap Cmd/Cntrl+Enter compile diagram
    const compileShortcutExtension = keymap.of([
      {
        key: "Mod-Enter",
        run: () => {
          onWrite();
          return true;
        },
      },
    ]);

    extensionsList.push(
      // Set precedence to override default
      Prec.highest([compileShortcutExtension]),
    );

    // Vim :w override to be compile diagram
    Vim.defineEx("write", "w", function () {
      onWrite();
    });
  }

  if (vimMode) {
    // NOTE: need to make sure vim keybindings have the highest precedence whenever we're in vim mode
    extensionsList.push(Prec.highest(vim()));
  }

  return (
    <div style={{ width: "100%", height: "100%", position: "relative" }}>
      <CodeMirror
        value={value}
        extensions={extensionsList}
        onChange={onChange}
        theme={darkMode ? "dark" : penroseTheme}
        // History reset https://github.com/uiwjs/react-codemirror/issues/405
        // Set in packages/editor/src/state/callbacks.ts
        basicSetup={{ history: codemirrorHistoryState }}
        width={width}
        height={height}
        maxHeight={maxHeight}
        maxWidth={maxWidth}
        minHeight={minHeight}
        minWidth={minWidth}
        readOnly={readOnly}
      />
      <div
        ref={statusBarRef}
        style={{ position: "absolute", bottom: 0, backgroundColor: "white" }}
      />
    </div>
  );
}
