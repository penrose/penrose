import { RenderStatic, showError } from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import toast from "react-hot-toast";
import { useRecoilCallback, useRecoilValue } from "recoil";
import {
  diagramState,
  WorkspaceMetadata,
  workspaceMetadataSelector,
} from "../state/atoms";
import BlueButton from "./BlueButton";

/**
 * (browser-only) Downloads any given exported SVG to the user's computer
 * @param svg
 * @param title the filename
 */
export const DownloadSVG = (
  svg: SVGSVGElement,
  title = "illustration"
): void => {
  const blob = new Blob([svg.outerHTML], {
    type: "image/svg+xml;charset=utf-8",
  });
  const url = URL.createObjectURL(blob);
  const downloadLink = document.createElement("a");
  downloadLink.href = url;
  downloadLink.download = `${title}.svg`;
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
};

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

  const downloadSvg = useRecoilCallback(({ snapshot }) => () => {
    if (canvasRef.current !== null) {
      const svg = canvasRef.current.firstElementChild as SVGSVGElement;
      if (svg !== null) {
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        DownloadSVG(svg, metadata.name);
      }
    }
  });

  const downloadPdf = useRecoilCallback(
    ({ snapshot }) => () => {
      if (canvasRef.current !== null) {
        const svg = canvasRef.current.firstElementChild as SVGSVGElement;
        if (svg !== null && state) {
          const metadata = snapshot.getLoadable(workspaceMetadataSelector)
            .contents as WorkspaceMetadata;
          const openedWindow = window.open(
            "",
            "PRINT",
            `height=${state.canvas.height},width=${state.canvas.width}`
          );
          if (openedWindow === null) {
            toast.error("Couldn't open popup to print");
            return;
          }
          openedWindow.document.write(
            `<!DOCTYPE html><head><title>${metadata.name}</title></head><body>`
          );
          openedWindow.document.write(svg.outerHTML);
          openedWindow.document.write("</body></html>");
          openedWindow.document.close();
          openedWindow.focus();
          openedWindow.print();
        }
      }
    },
    [state]
  );

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
      {state && (
        <div>
          <BlueButton onClick={downloadSvg}>SVG</BlueButton>
          <BlueButton onClick={downloadPdf}>PDF</BlueButton>
        </div>
      )}
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
