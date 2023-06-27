import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { compileDomain } from "@penrose/core";
import { editor } from "monaco-editor";
import { useEffect } from "react";
import { SetupSubstanceMonaco } from "./languages/SubstanceConfig.js";

export const defaultMonacoOptions: editor.IStandaloneEditorConstructionOptions =
  {
    automaticLayout: true,
    minimap: { enabled: false },
    wordWrap: "on",
    lineNumbers: "off",
    fontSize: 16,
    scrollbar: {
      handleMouseWheel: true,
    },
    scrollBeyondLastLine: false,
    renderLineHighlight: "none",
  };

const Listing = ({
  domain,
  substance,
  width,
  height,
  onChange,
  monacoOptions,
  readOnly,
}: {
  domain: string;
  substance: string;
  width: string;
  height: string;
  onChange?(value: string): void;
  monacoOptions?: editor.IStandaloneEditorConstructionOptions;
  readOnly?: boolean;
}) => {
  const env = compileDomain(domain).unsafelyUnwrap();
  const monaco = useMonaco();
  useEffect(() => {
    if (monaco) {
      if (env) {
        const dispose = SetupSubstanceMonaco(env)(monaco);
        return () => {
          // prevents duplicates
          dispose();
        };
      }
    }
  }, [monaco, env]);
  return (
    <MonacoEditor
      value={substance}
      width={width}
      height={height}
      onChange={onChange ? (v) => onChange(v ?? "") : undefined}
      defaultLanguage="substance"
      options={
        monacoOptions
          ? {
              ...defaultMonacoOptions,
              ...monacoOptions,
              readOnly: readOnly ?? true,
            }
          : defaultMonacoOptions
      }
    />
  );
};

export default Listing;
