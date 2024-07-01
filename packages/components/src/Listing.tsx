import { PenroseError, PenroseWarning } from "@penrose/core";
import { useEffect, useState } from "react";
import EditorPane from "./editing/EditorPane";
import { getDomainCache } from "./editing/hooks/domain/getDomainCache";
import { getSubstanceCache } from "./editing/hooks/substance/getSubstanceCache";

const Listing = ({
  domain,
  substance,
  width,
  height,
  darkMode,
  onChange,
  readOnly = true,
}: {
  domain: string;
  substance: string;
  width: string;
  height: string;
  onChange?(value: string): void;
  readOnly: boolean;
  darkMode: boolean;
}) => {
  const [error, setError] = useState<PenroseError | null>(null);
  const [warnings, setWarnings] = useState<PenroseWarning[]>([]);
  useEffect(() => {}, [domain, substance]);
  return (
    <EditorPane
      value={substance}
      readOnly={readOnly}
      onChange={onChange!}
      languageType={"substance"}
      domainCache={getDomainCache(domain)}
      substanceCache={getSubstanceCache(substance)}
      vimMode={false}
      error={error}
      warnings={warnings}
      codemirrorHistoryState={true}
      showCompileErrs={false}
    />
  );
};

export default Listing;
