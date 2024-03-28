import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { DomainEnv, compileDomain } from "@penrose/core";
import { editor } from "monaco-editor";
import { useEffect } from "react";
import { SetupDomainMonaco } from "./editing/languages/DomainConfig.js";
import { SetupStyleMonaco } from "./editing/languages/StyleConfig.js";
import { SetupSubstanceMonaco } from "./editing/languages/SubstanceConfig.js";

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
  src,
  width,
  height,
  language,
  onChange,
  monacoOptions,
  readOnly,
}: {
  src: string;
  domain?: string;
  language: "substance" | "domain" | "style";
  width: string;
  height: string;
  onChange?(value: string): void;
  monacoOptions?: editor.IStandaloneEditorConstructionOptions;
  readOnly?: boolean;
}) => {
  let env: DomainEnv | undefined;
  if (domain) {
    env = compileDomain(domain).unsafelyUnwrap();
  }
  const monaco = useMonaco();
  useEffect(() => {
    if (monaco) {
      if (language === "substance" && env) {
        const dispose = SetupSubstanceMonaco(env)(monaco);
        return () => {
          // prevents duplicates
          dispose();
        };
      } else if (language === "domain") {
        const dispose = SetupDomainMonaco(monaco);
        return () => {
          // prevents duplicates
          dispose();
        };
      } else if (language === "style") {
        const dispose = SetupStyleMonaco(monaco);
        return () => {
          // prevents duplicates
          dispose();
        };
      }
    }
  }, [monaco, env]);
  return (
    <MonacoEditor
      value={src}
      width={width}
      height={height}
      onChange={onChange ? (v) => onChange(v ?? "") : undefined}
      language={language}
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
