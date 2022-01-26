import { useEffect, useState } from "react";
import { FileDispatcher } from "../state/fileReducer";
import { fetchExamples, loadWorkspace } from "../state/fileSystemActions";
import { IExamples } from "../types/FileSystem";
import FileButton from "./FileButton";

export default function ExamplesPanel({
  dispatch,
}: {
  dispatch: FileDispatcher;
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
    </div>
  );
}
