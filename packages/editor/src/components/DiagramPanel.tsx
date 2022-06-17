import {
  RenderStatic,
  showError,
  stateConverged,
  stepStateSafe,
} from "@penrose/core";
import localforage from "localforage";
import { useEffect, useRef, useState } from "react";
import toast from "react-hot-toast";
import { useRecoilCallback, useRecoilState, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import {
  currentRogerState,
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
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const { state, error, metadata } = diagram;
  const [showEasterEgg, setShowEasterEgg] = useState(false);
  const { location, id } = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);

  const requestRef = useRef<number>();

  useEffect(() => {
    const cur = canvasRef.current;
    if (state !== null && cur !== null) {
      (async () => {
        // render the current frame
        const rendered = await RenderStatic(state, pathResolver);
        if (cur.firstElementChild) {
          cur.replaceChild(rendered, cur.firstElementChild);
        } else {
          cur.appendChild(rendered);
        }
      })();
    } else if (state === null && cur !== null) {
      cur.innerHTML = "";
    }
  }, [diagram.state]);

  useEffect(() => {
    // request the next frame if the diagram state updates
    requestRef.current = requestAnimationFrame(step);
    // Make sure the effect runs only once. Otherwise there might be other `step` calls running in the background causing race conditions
    return () => cancelAnimationFrame(requestRef.current!);
  }, [diagram.state]);

  const step = () => {
    if (state) {
      if (!stateConverged(state) && metadata.autostep) {
        const stepResult = stepStateSafe(state, metadata.stepSize);
        if (stepResult.isErr()) {
          setDiagram({
            ...diagram,
            error: stepResult.error,
          });
        } else {
          setDiagram({
            ...diagram,
            error: null,
            state: stepResult.value,
          });
        }
      }
    }
  };

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

  /**
   * Fetch url, but try local storage first using a name.
   * Update local storage if the file is fetched via url
   *
   * @param name The short name of the file to fetch
   * @param url The url to fetch, if not found locally
   * @returns Promise that resolves to the fetched string or undefined if the fetch failed
   */
  const fetchResource = async (
    name: string,
    url?: string
  ): Promise<string | undefined> => {
    const localFilePrefix = "localfile://" + id + "/";
    try {
      // Attempt to retrieve the resource from local storage
      const localImage = await localforage.getItem<string>(
        localFilePrefix + name
      );
      if (localImage) {
        return localImage;
      } else {
        // Return undefined if there is no url and not hit in local storage
        if (!url) return undefined;

        // Try to fetch it by url
        const httpResource = await fetch(url);
        // Update local storage and return the resource
        if (httpResource.ok) {
          const httpBody = httpResource.text();
          localforage.setItem(localFilePrefix + name, httpBody);
          return httpBody;
        } else {
          console.log(`HTTP status ${httpResource.status} for ${url}`);
          return undefined;
        }
      }
    } catch (e) {
      console.log(`Error fetching resource. Local name: ${name}, url: ${url}`);
      return undefined;
    }
  };

  const pathResolver = async (
    relativePath: string
  ): Promise<string | undefined> => {
    // Handle absolute URLs
    if (/^(http|https):\/\/[^ "]+$/.test(relativePath)) {
      const url = new URL(relativePath).href;
      return fetchResource(url, url);
    }

    // Handle relative paths
    switch (location.kind) {
      case "example": {
        return fetchResource(
          relativePath,
          new URL(relativePath, location.root).href
        );
      }
      case "roger": {
        if (rogerState.kind === "connected") {
          const { ws } = rogerState;
          return new Promise((resolve /*, reject*/) => {
            const token = uuid();
            ws.addEventListener("message", (e) => {
              const parsed = JSON.parse(e.data);
              if (parsed.kind === "file_change" && parsed.token === token) {
                return resolve(parsed.contents);
              }
            });
            ws.send(
              JSON.stringify({
                kind: "retrieve_file_from_style",
                relativePath,
                stylePath: location.style,
                token,
              })
            );
          });
        }
      }
      // TODO: publish images in the gist
      case "gist":
        return undefined;
      case "local": {
        return fetchResource(relativePath);
      }
    }
  };

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
          height="600"
          src="https://www.youtube.com/embed/ofFLYfUEPVE?start=9&amp;autoplay=1"
          title="YouTube video player"
          frameBorder="0"
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        ></iframe>
      )}
    </div>
  );
}
