import MonacoEditor, { useMonaco } from "@monaco-editor/react";
import { editor } from "monaco-editor";
import { useEffect } from "react";
import { SubstanceConfig } from "./editing/languages/SubstanceConfig";

export const monacoOptions: editor.IStandaloneEditorConstructionOptions = {
  automaticLayout: true,
  readOnly: true,
  minimap: { enabled: false },
  wordWrap: "on",
  lineNumbers: "off",
  fontSize: 16,
  scrollbar: {
    handleMouseWheel: false,
  },
  scrollBeyondLastLine: false,
  renderLineHighlight: "none",
};

const Listing = ({
  value,
  width,
  height,
}: {
  value: string;
  width: string;
  height: string;
}) => {
  const monaco = useMonaco();
  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: "substance" });
      monaco.languages.setLanguageConfiguration("substance", SubstanceConfig);
    }
  }, [monaco]);
  return (
    <MonacoEditor
      value={value}
      width={width}
      height={height}
      defaultLanguage="substance"
      options={monacoOptions}
    />
  );
};

export default Listing;
