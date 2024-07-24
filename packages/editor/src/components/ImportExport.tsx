import { FileUploader } from "react-drag-drop-files";
import toast from "react-hot-toast";
import { useRecoilState } from "recoil";
import { v4 as uuid } from "uuid";
import {
  WorkspaceLocation,
  currentWorkspaceState,
  diagramMetadataSelector,
} from "../state/atoms.js";
import { isCleanWorkspace, useCompileDiagram } from "../state/callbacks.js";
import { BodyText } from "./Elements.js";

export default function ImportExport() {
  const [, setDiagramMetadata] = useRecoilState(diagramMetadataSelector);
  const compileDiagram = useCompileDiagram();
  const [currentWorkspace, setWorkspace] = useRecoilState(
    currentWorkspaceState,
  );

  const handleChange = (svg: File) => {
    if (
      !isCleanWorkspace(currentWorkspace) &&
      !confirm(
        "You have unsaved changes. Are you sure you want to load this SVG?",
      )
    ) {
      return;
    }

    const reader = new FileReader();
    reader.readAsText(svg);
    reader.onabort = () => console.log("file reading was aborted");
    reader.onerror = () => console.log("file reading has failed");
    reader.onload = async () => {
      const svgText = reader.result?.toString();
      if (!svgText) {
        return;
      }

      const parser = new DOMParser();
      const xmlDoc = parser.parseFromString(svgText, "text/xml");
      const styElem = xmlDoc.getElementsByTagName("sty");
      const subElem = xmlDoc.getElementsByTagName("sub");
      const dslElem = xmlDoc.getElementsByTagName("dsl");
      const variationElem = xmlDoc.getElementsByTagName("variation");

      if (
        variationElem.length === 0 ||
        styElem.length === 0 ||
        subElem.length === 0 ||
        dslElem.length === 0
      ) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose.",
        );
        return;
      }

      // put imported workspace into draft state
      // prevents overwriting existing workspaces
      setWorkspace({
        ...currentWorkspace,
        metadata: {
          ...currentWorkspace.metadata,
          location: { kind: "local", changesMade: false } as WorkspaceLocation,
          id: uuid(),
          name: svg.name.replace(".svg", ""),
        },
        files: {
          substance: {
            ...currentWorkspace.files.substance,
            contents: (subElem[0].textContent ?? "").trim(),
          },
          style: {
            ...currentWorkspace.files.style,
            contents: (styElem[0].textContent ?? "").trim(),
          },
          domain: {
            ...currentWorkspace.files.domain,
            contents: (dslElem[0].textContent ?? "").trim(),
          },
        },
      });

      setDiagramMetadata((metadata) => ({
        ...metadata,
        variation: (variationElem[0].textContent ?? "").trim(),
      }));

      await compileDiagram();

      toast.success("Sucessfully uploaded SVG to editor");
    };
  };

  return (
    <FileUploader
      handleChange={handleChange}
      name="file"
      types={["SVG"]}
      multiple={false}
      label="Upload or drop a Penrose exported SVG here"
    >
      <div
        style={{ border: "2px dashed", borderColor: "darkgrey", padding: 10 }}
      >
        <BodyText>
          <span style={{ textDecoration: "underline" }}>Upload</span> or drop a
          Penrose exported SVG here
        </BodyText>
      </div>
    </FileUploader>
  );
}
