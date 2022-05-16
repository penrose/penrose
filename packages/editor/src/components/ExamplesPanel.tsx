import { useEffect, useState } from "react";
import { useLoadWorkspace, useOpenFileInWorkspace } from "../state/atoms";
import { fetchExamples } from "../state/fileSystemActions";
import { IExamples } from "../types/FileSystem";
import FileButton from "./FileButton";

export default function ExamplesPanel() {
  const [examples, setExamples] = useState<IExamples | null>();
  const loadWorkspace = useLoadWorkspace();
  useEffect(() => {
    (async () => {
      const ex = await fetchExamples();
      setExamples(ex);
    })();
  }, []);
  const openFileInWorkspace = useOpenFileInWorkspace();
  if (!examples) {
    return (
      <div>
        <p>loading...</p>
      </div>
    );
  }
  return (
    <div>
      <h1>Examples</h1>
      <h2>Workspaces</h2>
      <div>
        {Object.entries(examples.trios).map(([id, trio]) => (
          <FileButton
            key={id}
            name={trio.name}
            onClick={() => loadWorkspace(trio)}
          />
        ))}
      </div>
      <h2>By Domain</h2>
      <div>
        {Object.values(examples.domains).map((domain) => (
          <div key={domain.id}>
            <h3>{domain.name}</h3>
            <FileButton
              key={domain.id}
              name={domain.name}
              onClick={() => openFileInWorkspace(domain)}
            />
            <div>
              <h4>Styles</h4>
              {Object.values(examples.styles)
                .filter((style) => style.domain.id === domain.id)
                .map((style) => (
                  <FileButton
                    key={style.id}
                    name={style.name}
                    onClick={() => openFileInWorkspace(style)}
                  />
                ))}
            </div>
            <div>
              <h4>Substances</h4>
              {Object.values(examples.substances)
                .filter((substance) => substance.domain.id === domain.id)
                .map((substance) => (
                  <FileButton
                    key={substance.id}
                    name={substance.name}
                    onClick={() => openFileInWorkspace(substance)}
                  />
                ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
