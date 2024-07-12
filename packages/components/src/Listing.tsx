import { PenroseError, PenroseWarning } from "@penrose/core";
import { useEffect, useState } from "react";
import EditorPane from "./editing/EditorPane";
import { getDomainCache } from "./editing/hooks/domain/getDomainCache";
import { getSubstanceCache } from "./editing/hooks/substance/getSubstanceCache";

const Listing = ({
  domain,
  src,
  width,
  height,
  darkMode,
  onChange,
  language = "substance",
  readOnly = true,
}: {
  domain: string;
  src: string;
  width: string;
  height: string;
  onChange?(value: string): void;
  readOnly: boolean;
  darkMode: boolean;
  language: "domain" | "style" | "substance";
}) => {
  const [error, setError] = useState<PenroseError | null>(null);
  const [warnings, setWarnings] = useState<PenroseWarning[]>([]);
  useEffect(() => {}, [domain, src]);
  return (
    <EditorPane
      value={src}
      readOnly={readOnly}
      onChange={onChange!}
      languageType={language}
      domainCache={getDomainCache(domain)}
      substanceCache={getSubstanceCache(src)}
      darkMode={darkMode}
      vimMode={false}
      error={error}
      warnings={warnings}
      codemirrorHistoryState={true}
      showCompileErrs={false}
      width={width}
      height={height}
    />
  );
};

export default Listing;
