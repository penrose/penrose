import {
  PenroseState,
  RenderInteractive,
  RenderStatic,
  showError,
} from "@penrose/core";
import { useCallback, useEffect, useRef } from "react";
import { State } from "../reducer";
import { DownloadSVG } from "../Util";
import BlueButton from "./BlueButton";

export default function PreviewPane({
  state,
  convergeRenderState,
  onResample,
}: {
  state: State;
  convergeRenderState(state: PenroseState): void;
  onResample(): void;
}) {
  const canvasRef = useRef<HTMLDivElement>(null);
  const svg = useCallback(() => {
    if (state.currentInstance.state) {
      const rendered = RenderStatic(state.currentInstance.state);
      DownloadSVG(rendered);
    }
  }, [state]);
  useEffect(() => {
    if (state.currentInstance.state) {
      const cur = canvasRef.current;
      const rendered = RenderInteractive(
        state.currentInstance.state,
        convergeRenderState
      );
      if (cur) {
        if (cur.firstChild) {
          cur.replaceChild(rendered, cur.firstChild);
        } else {
          cur.appendChild(rendered);
        }
      }
    }
  }, [state]);
  return (
    <div>
      <div
        style={{
          position: "absolute",
          padding: "1em",
          right: 0,
          display: "flex",
        }}
      >
        <BlueButton onClick={onResample}>resample</BlueButton>
        <BlueButton onClick={svg}>SVG</BlueButton>
      </div>
      <div
        style={{
          width: "100%",
          height: "100%",
          overflow: "auto",
          backgroundColor: "#ffffff",
          border: "1px solid gray",
        }}
        ref={canvasRef}
      />
      {state.currentInstance.err && (
        <div
          style={{
            position: "absolute",
            bottom: 0,
            backgroundColor: "#ffdada",
            maxHeight: "400px",
            maxWidth: "100%",
            overflow: "auto",
            padding: "10px",
            boxSizing: "border-box",
          }}
        >
          <pre>{showError(state.currentInstance.err).toString()}</pre>
        </div>
      )}
    </div>
  );
}
