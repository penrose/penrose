import { FileUploader } from "react-drag-drop-files";
import toast from "react-hot-toast";
import { useRecoilState } from "recoil";
import styled from "styled-components";
import { v4 as uuid } from "uuid";
import {
  WorkspaceLocation,
  currentWorkspaceState,
  diagramMetadataSelector,
} from "../state/atoms.js";
import {
  isCleanWorkspace,
  useCompileDiagram,
  useCopyToClipboard,
  useDownloadPdf,
  useDownloadPng,
  useDownloadSvg,
  useDownloadSvgTex,
  useDownloadTrio,
} from "../state/callbacks.js";
import { SettingHeader, SettingText } from "./SettingElements.js";

const DropZone = styled.div`
  border-color: #989898;
  stroke-width: 2px;
  border-width: 1.8px;
  border-style: dashed;
  border-radius: 10px;
  padding: 8px;
  text-align: center;
  margin: 10px;
  color: #353538;
`;

const ExportCard = styled.div`
  background-color: #f5f5f5;
  border-radius: 10px;
  padding: 20px;
  margin: 10px;
  color: #353538;

  &:hover {
    cursor: pointer;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }
`;

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
    <div>
      <SettingHeader>Import</SettingHeader>
      <FileUploader
        handleChange={handleChange}
        name="file"
        types={["SVG"]}
        multiple={false}
        label="Upload or drop a Penrose exported SVG here"
      >
        <DropZone>
          <SettingText>
            <span style={{ textDecoration: "underline" }}>Upload</span> or drop
            a Penrose exported SVG here
          </SettingText>
        </DropZone>
      </FileUploader>
      <SettingHeader>Export</SettingHeader>
      <ExportCard onClick={useDownloadSvgTex()}>TeX SVG</ExportCard>
      <ExportCard onClick={useDownloadSvg()}>Penrose SVG</ExportCard>
      <ExportCard onClick={useDownloadTrio()}>Penrose Trio</ExportCard>
      <ExportCard onClick={useDownloadPng()}>PNG</ExportCard>
      <ExportCard onClick={useDownloadPdf()}>PDF</ExportCard>
      <ExportCard onClick={useCopyToClipboard()}>Copy to Clipboard</ExportCard>
    </div>
  );
}
