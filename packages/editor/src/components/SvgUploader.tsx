import { FileUploader } from "react-drag-drop-files";
import toast from "react-hot-toast";
import { useRecoilState } from "recoil";
import {
  diagramMetadataSelector,
  fileContentsSelector,
} from "../state/atoms.js";
import { useCompileDiagram } from "../state/callbacks.js";

export default function SvgUploader() {
  const setDomain = useRecoilState(fileContentsSelector("domain"))[1];
  const setSubstance = useRecoilState(fileContentsSelector("substance"))[1];
  const setStyle = useRecoilState(fileContentsSelector("style"))[1];
  const [diagramMetadata, setDiagramMetadata] = useRecoilState(
    diagramMetadataSelector,
  );
  const compileDiagram = useCompileDiagram();

  const handleChange = (svg: File) => {
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

      if (variationElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose.",
        );
        return;
      }

      setDiagramMetadata((metadata) => ({
        ...metadata,
        variation: (variationElem[0].textContent ?? "").trim(),
      }));

      if (styElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose.",
        );
        return;
      }

      setStyle({
        name: "SVG import",
        contents: (styElem[0].textContent ?? "").trim(),
      });

      if (subElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose.",
        );
        return;
      }

      setSubstance({
        name: "SVG import",
        contents: (subElem[0].textContent ?? "").trim(),
      });

      if (dslElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose.",
        );
        return;
      }

      setDomain({
        name: "SVG import",
        contents: (dslElem[0].textContent ?? "").trim(),
      });

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
        <p>
          <span style={{ textDecoration: "underline" }}>Upload</span> or drop a
          Penrose exported SVG here
        </p>
      </div>
    </FileUploader>
  );
}
