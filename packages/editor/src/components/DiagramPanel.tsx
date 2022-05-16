import { RenderStatic } from "@penrose/core";
import { useEffect, useRef } from "react";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms";

export default function DiagramPanel() {
  const canvasRef = useRef<HTMLDivElement>(null);
  const { state, error, variation } = useRecoilValue(diagramState);

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
  }, [state]);

  return (
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
  );
}
