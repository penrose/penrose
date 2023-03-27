import {
  PenroseState,
  RenderInteractive,
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
  DiagramMetadata,
  diagramMetadataSelector,
  diagramState,
  fileContentsSelector,
  ProgramFile,
  RogerState,
  WorkspaceMetadata,
  workspaceMetadataSelector,
} from "../state/atoms";
import BlueButton from "./BlueButton";

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
  { id }: WorkspaceMetadata,
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

export const pathResolver = async (
  relativePath: string,
  rogerState: RogerState,
  workspace: WorkspaceMetadata
): Promise<string | undefined> => {
  const { location } = workspace;

  // Handle absolute URLs
  if (/^(http|https):\/\/[^ "]+$/.test(relativePath)) {
    const url = new URL(relativePath).href;
    return fetchResource(url, workspace, url);
  }

  // Handle relative paths
  switch (location.kind) {
    case "example": {
      return fetchResource(
        relativePath,
        workspace,
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
      return fetchResource(relativePath, workspace);
    }
  }
};

/**
 * (browser-only) Downloads any given exported SVG to the user's computer
 * @param svg
 * @param title the filename
 */
export const DownloadSVG = (
  svg: SVGSVGElement,
  title = "illustration",
  dslStr: string,
  subStr: string,
  styleStr: string,
  versionStr: string,
  variationStr: string
): void => {
  SVGaddCode(svg, dslStr, subStr, styleStr, versionStr, variationStr);
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

/**
 * Given an SVG, program triple, and version and variation strings,
 * appends penrose tags to the SVG so the SVG can be reuploaded and edited.
 *
 * @param svg
 * @param dslStr the domain file
 * @param subStr the substance file
 * @param styleStr the style file
 * @param versionStr
 * @param variationStr
 */
const SVGaddCode = (
  svg: SVGSVGElement,
  dslStr: string,
  subStr: string,
  styleStr: string,
  versionStr: string,
  variationStr: string
): void => {
  svg.setAttribute("penrose", "0");

  // Create custom <penrose> tag to store metadata
  const metadata = document.createElementNS(
    "https://penrose.cs.cmu.edu/metadata",
    "penrose"
  );

  // Create <version> tag for penrose version
  const version = document.createElementNS(
    "https://penrose.cs.cmu.edu/version",
    "version"
  );
  version.insertAdjacentText("afterbegin", versionStr);

  // Create <variation> tag for variation string
  const variation = document.createElementNS(
    "https://penrose.cs.cmu.edu/variation",
    "variation"
  );
  variation.insertAdjacentText("afterbegin", variationStr);

  // Create <sub> tag to store .substance code
  const substance = document.createElementNS(
    "https://penrose.cs.cmu.edu/substance",
    "sub"
  );
  substance.insertAdjacentText("afterbegin", subStr);

  // Create <sty> tag to store .style code
  const style = document.createElementNS(
    "https://penrose.cs.cmu.edu/style",
    "sty"
  );
  style.insertAdjacentText("afterbegin", styleStr);

  // Create <dsl> tag to store .domain code
  const dsl = document.createElementNS("https://penrose.cs.cmu.edu/dsl", "dsl");
  dsl.insertAdjacentText("afterbegin", dslStr);

  // Add these new tags under the <penrose> metadata tag
  metadata.appendChild(version);
  metadata.appendChild(variation);
  metadata.appendChild(substance);
  metadata.appendChild(style);
  metadata.appendChild(dsl);

  // Add the <penrose> metadata tag to the parent <svg> tag
  svg.appendChild(metadata);
};

/**
 * (browser-only) Downloads any given exported PNG to the user's computer
 * @param svg
 * @param title the filename
 * @param width canvas and SVG width
 * @param height canvas and SVG height
 */
export const DownloadPNG = (
  svg: SVGSVGElement,
  title = "illustration",
  width: number,
  height: number,
  scale: number
): void => {
  // duplicate node to set concrete dimensions
  const svgNode = svg.cloneNode(true) as SVGSVGElement;
  const canvas = document.createElement("canvas");
  const ctx = canvas.getContext("2d")!;
  // NOTE: need to set _both_ the SVG node and canvas dimensions to render properly
  const scaledWidth = width * scale;
  const scaledHeight = height * scale;
  canvas.width = scaledWidth;
  canvas.height = scaledHeight;
  svgNode.setAttribute("width", scaledWidth.toString());
  svgNode.setAttribute("height", scaledHeight.toString());
  const data = new XMLSerializer().serializeToString(svgNode);
  const DOMURL = window.URL || window.webkitURL || window;

  const img = new Image();
  const svgBlob = new Blob([data], {
    type: "image/svg+xml;charset=utf-8",
  });
  const url = DOMURL.createObjectURL(svgBlob);

  img.onload = function () {
    ctx.drawImage(img, 0, 0);
    DOMURL.revokeObjectURL(url);

    const imgURI = canvas
      .toDataURL("image/png")
      .replace("image/png", "image/octet-stream");
    const downloadLink = document.createElement("a");
    downloadLink.href = imgURI;
    downloadLink.download = title;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
  };
  img.src = url;
};

export default function DiagramPanel() {
  const canvasRef = useRef<HTMLDivElement>(null);
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const { state, error, metadata } = diagram;
  const [showEasterEgg, setShowEasterEgg] = useState(false);
  const { interactive } = useRecoilValue(diagramMetadataSelector);
  const workspace = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);

  const requestRef = useRef<number>();

  useEffect(() => {
    const cur = canvasRef.current;
    if (state !== null && cur !== null) {
      (async () => {
        // render the current frame
        const rendered = interactive
          ? await RenderInteractive(
              state,
              (newState: PenroseState) => {
                setDiagram({
                  ...diagram,
                  state: newState,
                });
                step();
              },
              (path) => pathResolver(path, rogerState, workspace),
              "diagramPanel"
            )
          : await RenderStatic(
              state,
              (path) => pathResolver(path, rogerState, workspace),
              "diagramPanel"
            );
        rendered.setAttribute("width", "100%");
        rendered.setAttribute("height", "100%");
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
        const diagram = snapshot.getLoadable(diagramMetadataSelector)
          .contents as DiagramMetadata;
        const domain = snapshot.getLoadable(fileContentsSelector("domain"))
          .contents as ProgramFile;
        const substance = snapshot.getLoadable(
          fileContentsSelector("substance")
        ).contents as ProgramFile;
        const style = snapshot.getLoadable(fileContentsSelector("style"))
          .contents as ProgramFile;
        DownloadSVG(
          svg,
          metadata.name,
          domain.contents,
          substance.contents,
          style.contents,
          metadata.editorVersion.toString(),
          diagram.variation
        );
      }
    }
  });

  const downloadPng = useRecoilCallback(({ snapshot }) => async () => {
    if (canvasRef.current !== null) {
      const svg = canvasRef.current.firstElementChild as SVGSVGElement;
      if (svg !== null) {
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        const filename = `${metadata.name}.png`;
        if (diagram.state) {
          const { canvas: canvasDims } = diagram.state;
          const { width, height } = canvasDims;
          DownloadPNG(svg, filename, width, height, 1);
        }
      }
    }
  });

  const downloadPdf = useRecoilCallback(
    ({ snapshot }) =>
      () => {
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
    <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          maxHeight: "100%",
          width: "100%",
        }}
      >
        {state === null && (
          <span onClick={() => setShowEasterEgg((s) => !s)}>
            press compile to see diagram
          </span>
        )}
        {state && (
          <div style={{ display: "flex" }}>
            <BlueButton onClick={downloadSvg}>SVG</BlueButton>
            <BlueButton onClick={downloadPng}>PNG</BlueButton>
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
              minHeight: "100px",
              overflow: "auto",
              padding: "10px",
              boxSizing: "border-box",
            }}
          >
            <span
              style={{ fontWeight: "bold", color: "#ee4e4e", fontSize: 14 }}
            >
              error ({error.errorType})
            </span>
            <pre>{showError(error).toString()}</pre>
          </div>
        )}
        <div
          style={{
            display: "flex",
            minHeight: "60%",
            maxHeight: "100%",
            justifyContent: "center",
          }}
          ref={canvasRef}
        />

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
    </div>
  );
}
