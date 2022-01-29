// TODO:
//  resample
// recompile (play button unicode) - or auto recompile
// switch trio

import { RenderStatic } from "@penrose/core";
import { useEffect, useRef } from "react";
import { DiagramFilePointer, StateFile } from "../types/FileSystem";

// error box
export default function DiagramPanel({
  filePointer,
  fileContents,
}: {
  filePointer: DiagramFilePointer;
  fileContents: StateFile;
}) {
  const state = fileContents.contents;
  const canvasRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const cur = canvasRef.current;
    if (state !== null && cur !== null) {
      (async () => {
        const rendered = await RenderStatic(state, async () => null);
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
    </div>
  );
}
