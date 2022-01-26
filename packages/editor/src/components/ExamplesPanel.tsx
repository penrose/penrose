import { useEffect, useState } from "react";
import { FileDispatcher } from "../state/fileReducer";
import {
  fetchExamples,
  loadWorkspace,
  openFileInWorkspace,
} from "../state/fileSystemActions";
import { IExamples, IWorkspace } from "../types/FileSystem";
import FileButton from "./FileButton";

export default function ExamplesPanel({
  dispatch,
  workspace,
}: {
  dispatch: FileDispatcher;
  workspace: IWorkspace;
}) {
  const [examples, setExamples] = useState<IExamples | null>();
  useEffect(() => {
    (async () => {
      const ex = await fetchExamples();
      setExamples(ex);
    })();
  }, []);
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
            onClick={() => loadWorkspace(dispatch, trio)}
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
              onClick={() => openFileInWorkspace(dispatch, workspace, domain)}
            />
            <div>
              <h4>Styles</h4>
              {Object.values(examples.styles)
                .filter((style) => style.domain.id === domain.id)
                .map((style) => (
                  <FileButton
                    key={style.id}
                    name={style.name}
                    onClick={() =>
                      openFileInWorkspace(dispatch, workspace, style)
                    }
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
                    onClick={() =>
                      openFileInWorkspace(dispatch, workspace, substance)
                    }
                  />
                ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
