import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { compileDomain } from "@penrose/core";
import { editor } from "monaco-editor";
import { useEffect } from "react";
import { SetupSubstanceMonaco } from "./editing/languages/SubstanceConfig";

export const defaultMonacoOptions: editor.IStandaloneEditorConstructionOptions =
  {
    automaticLayout: true,
    readOnly: true,
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
  monacoOptions,
}: {
  domain: string;
  substance: string;
  width: string;
  height: string;
  monacoOptions?: editor.IStandaloneEditorConstructionOptions;
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
      defaultLanguage="substance"
      options={
        monacoOptions
          ? { ...defaultMonacoOptions, ...monacoOptions }
          : defaultMonacoOptions
      }
    />
  );
};

export default Listing;
