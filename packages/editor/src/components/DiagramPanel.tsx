import { RenderStatic, showError } from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms";

export default function DiagramPanel() {
  const canvasRef = useRef<HTMLDivElement>(null);
  const { state, error, metadata } = useRecoilValue(diagramState);
  const [showEasterEgg, setShowEasterEgg] = useState(false);

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
    } else if (state === null && cur !== null) {
      cur.innerHTML = "";
    }
  }, [state]);

  return (
    <div>
      {state === null && (
        <span onClick={() => setShowEasterEgg((s) => !s)}>
          press compile to see diagram
        </span>
      )}
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
      {error && (
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
          <span style={{ fontWeight: "bold", color: "#ee4e4e", fontSize: 14 }}>
            error ({error.errorType})
          </span>
          <pre>{showError(error).toString()}</pre>
        </div>
      )}
      {showEasterEgg && (
        <iframe
          width="100%"
          height="300"
          src="https://www.youtube.com/embed/6j928wBZ_Bo?controls=0&amp;start=9&amp;autoplay=1"
          title="YouTube video player"
          frameBorder="0"
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        ></iframe>
      )}
    </div>
  );
}
