import { useEffect, useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import { currentStyleResourcesState } from "../state/atoms";
import BlueButton from "./BlueButton";
import FileButton from "./FileButton";

const StyleResourceUploader = () => {
  const [styleResources, setStyleResources] = useRecoilState(
    currentStyleResourcesState,
  );
  const [currentFiles] = useState<[string, string][]>([]);
  const [allFilesReady, setAllFilesReady] = useState(false);

  useEffect(() => {
    if (allFilesReady) {
      setStyleResources(
        styleResources.merge(
          currentFiles.map(([name, text]) => [name, { contents: text }]),
        ),
      );
      while (currentFiles.length !== 0) currentFiles.pop();
      setAllFilesReady(false);
    }
  }, [allFilesReady]);
  return (
    <div>
      <BlueButton
        style={{
          marginTop: "10px",
          marginBottom: "10px",
        }}
        onClick={() => {
          document.getElementById("actual-file-input")?.click();
        }}
      >
        + Add Resources
      </BlueButton>
      <input
        id="actual-file-input"
        type="file"
        accept="image/svg+xml"
        hidden
        multiple
        onChange={(e) => {
          const files = [...(e.target as HTMLInputElement).files!];
          files.forEach((f) => {
            const name = f.name;
            const text = f.text();
            text.then((t) => {
              currentFiles.push([name, t]);
              if (currentFiles.length === files.length) {
                setAllFilesReady(true);
              }
            });
          });
        }}
      />
    </div>
  );
};

const StyleResource = ({ resourceName }: { resourceName: string }) => {
  const [styleResources, setStyleResources] = useRecoilState(
    currentStyleResourcesState,
  );
  const resource = styleResources.get(resourceName);
  if (resource === undefined) {
    throw new Error("cannot locate resource with key = " + resourceName);
  }
  return (
    <FileButton
      key={resourceName}
      onClick={() => {}}
      onDelete={() => {
        const confirmed = confirm(`Delete resource ${resourceName}?`);
        if (confirmed) {
          setStyleResources(styleResources.delete(resourceName));
        }
      }}
    >
      {resourceName}
    </FileButton>
  );
};

export const StyleResourceEditor = () => {
  const styleResources = useRecoilValue(currentStyleResourcesState);
  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        maxHeight: "100%",
        width: "100%",
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          maxHeight: "100%",
          width: "100%",
          justifyContent: "space-between",
        }}
      >
        <div />
        <StyleResourceUploader />
      </div>
      <div>
        {Array.from(styleResources.keys()).map((key) => (
          <StyleResource resourceName={key} key={key} />
        ))}
      </div>
      {styleResources.size === 0 && (
        <span>No external resources have been loaded.</span>
      )}
    </div>
  );
};
