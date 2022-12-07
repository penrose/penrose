import { FileUploader } from "react-drag-drop-files";
import toast from "react-hot-toast";
import { useRecoilState } from "recoil";
import { fileContentsSelector } from "../state/atoms";

export default function SvgUploader() {
  const setDomain = useRecoilState(fileContentsSelector("domain"))[1];
  const setSubstance = useRecoilState(fileContentsSelector("substance"))[1];
  const setStyle = useRecoilState(fileContentsSelector("style"))[1];

  const handleChange = (svg: File) => {
    const reader = new FileReader();
    reader.readAsText(svg);
    reader.onabort = () => console.log("file reading was aborted");
    reader.onerror = () => console.log("file reading has failed");
    reader.onload = () => {
      const svgText = reader.result?.toString();
      if (!svgText) {
        return;
      }

      const parser = new DOMParser();
      const xmlDoc = parser.parseFromString(svgText, "text/xml");
      const styElem = xmlDoc.getElementsByTagName("sty");
      const subElem = xmlDoc.getElementsByTagName("sub");
      const dslElem = xmlDoc.getElementsByTagName("dsl");

      if (styElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose."
        );
        return;
      }

      setStyle({
        name: "SVG import",
        contents: styElem[0].innerHTML.trim(),
      });

      if (subElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose."
        );
        return;
      }

      setSubstance({
        name: "SVG import",
        contents: subElem[0].innerHTML.trim(),
      });

      if (dslElem.length === 0) {
        toast.error(
          "Could not load SVG. Make sure the SVG was exported from Penrose."
        );
        return;
      }

      setDomain({
        name: "SVG import",
        contents: dslElem[0].innerHTML.trim(),
      });

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
