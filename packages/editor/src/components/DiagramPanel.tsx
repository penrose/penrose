// TODO:
//  resample
// recompile (play button unicode) - or auto recompile
// switch trio

import { RenderStatic, showError } from "@penrose/core";
import { useEffect, useRef } from "react";
import { useRecompileDiagram, useResampleDiagram } from "../state/atoms";
import { DiagramFile, DiagramFilePointer } from "../types/FileSystem";
import BlueButton from "./BlueButton";

// error box
export default function DiagramPanel({
  filePointer,
  fileContents,
}: {
  filePointer: DiagramFilePointer;
  fileContents: DiagramFile;
}) {
  const state = fileContents.contents;
  const canvasRef = useRef<HTMLDivElement>(null);
  const recompileDiagram = useRecompileDiagram();
  const resampleDiagram = useResampleDiagram();
  useEffect(() => {
    // Auto compile if attempting to display unrendered diagram
    // (occurs when loading from anywhere!)
    if (
      fileContents.contents === null &&
      fileContents.metadata.error === null
    ) {
      recompileDiagram(filePointer);
    }
  }, [fileContents, filePointer]);
  useEffect(() => {
    const cur = canvasRef.current;
    if (state !== null && cur !== null) {
      (async () => {
        const rendered = await RenderStatic(state, async () => undefined);
        if (cur.firstElementChild) {
          cur.replaceChild(rendered, cur.firstElementChild);
        } else {
          cur.appendChild(rendered);
        }
      })();
    }
  }, [fileContents]);
  return (
    <div>
      <div style={{ display: "flex", flexDirection: "row", padding: "10px" }}>
        <BlueButton>
          autostep ({fileContents.metadata.autostep ? "on" : "off"})
        </BlueButton>
        <BlueButton onClick={() => recompileDiagram(filePointer)}>
          recompile ▶
        </BlueButton>
        <BlueButton onClick={() => resampleDiagram(filePointer)}>
          resample ↺
        </BlueButton>
      </div>
      <div
        style={{
          width: "100%",
          height: "100%",
          overflow: "auto",
          backgroundColor: "#FFFFFF",
          border: "1px solid gray",
        }}
        ref={canvasRef}
      />
      {fileContents.metadata.error && (
        <div
          style={{
            bottom: 0,
            backgroundColor: "#ffdada",
            maxHeight: "100%",
            maxWidth: "100%",
            overflow: "auto",
            padding: "10px",
            boxSizing: "border-box",
          }}
        >
          <pre>{showError(fileContents.metadata.error).toString()}</pre>
        </div>
      )}
    </div>
  );
}
