import FileSaver from "file-saver";
import JSZip from "jszip";
import localforage from "localforage";
import { optimize } from "svgo";
import { v4 as uuid } from "uuid";
import { ProgramFile, RogerState, WorkspaceMetadata } from "../state/atoms";

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
  url?: string,
): Promise<string | undefined> => {
  const localFilePrefix = "localfile://" + id + "/";
  try {
    // Attempt to retrieve the resource from local storage
    const localImage = await localforage.getItem<string>(
      localFilePrefix + name,
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
  workspace: WorkspaceMetadata,
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
      return location.resolver(relativePath);
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
            }),
          );
        });
      } else {
        return undefined;
      }
    }
    // TODO: publish images in the gist
    case "gist":
      return undefined;
    case "local": {
      const { resolver } = location;
      return resolver
        ? resolver(relativePath)
        : fetchResource(relativePath, workspace);
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
  variationStr: string,
): void => {
  SVGaddCode(svg, dslStr, subStr, styleStr, versionStr, variationStr);
  // optimize the svg output
  const svgStr = optimize(svg.outerHTML, {
    plugins: ["inlineStyles", "prefixIds"],
    path: title,
  }).data;
  const blob = new Blob([svgStr], {
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
  variationStr: string,
): void => {
  // Create custom <penrose> tag to store metadata, or grab it if it already exists
  const metadataQuery = svg.querySelector("penrose");
  let metadata: Element;

  if (metadataQuery === null) {
    metadata = document.createElementNS(
      "https://penrose.cs.cmu.edu/metadata",
      "penrose",
    );
    // Add the <penrose> metadata tag to the parent <svg> tag
    svg.appendChild(metadata);
  } else {
    metadata = metadataQuery!;
  }

  // Create <version> tag for penrose version
  const version = document.createElementNS(
    "https://penrose.cs.cmu.edu/version",
    "version",
  );
  version.insertAdjacentText("afterbegin", versionStr);

  // Create <variation> tag for variation string
  const variation = document.createElementNS(
    "https://penrose.cs.cmu.edu/variation",
    "variation",
  );
  variation.insertAdjacentText("afterbegin", variationStr);

  // Create <sub> tag to store .substance code
  const substance = document.createElementNS(
    "https://penrose.cs.cmu.edu/substance",
    "sub",
  );
  substance.insertAdjacentText("afterbegin", subStr);

  // Create <sty> tag to store .style code
  const style = document.createElementNS(
    "https://penrose.cs.cmu.edu/style",
    "sty",
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
  scale: number,
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

/**
 * Saves a Penrose Trio as a zip file
 *
 * @param fileInfos array of ProgramFile containing domain, substance, style progs
 * @param zipTitle name to save zip file as
 */
export const zipTrio = (fileInfos: ProgramFile[], zipTitle: string) => {
  // https://stackoverflow.com/questions/8608724/how-to-zip-files-using-javascript
  const zip = new JSZip();
  fileInfos.map((file) => zip.file(`${zipTitle}${file.name}`, file.contents));
  zip.generateAsync({ type: "blob" }).then(function (content) {
    FileSaver.saveAs(content, `${zipTitle}.zip`);
  });
};
