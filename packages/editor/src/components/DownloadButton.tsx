import {
  downloadPdf,
  downloadPng,
  downloadSvg,
  downloadSvgTex,
  useDownloadTrio,
} from "../state/callbacks.js";
import DropdownButton, { DropdownItem } from "./DropdownButton.js";

export default function DownloadButton() {
  // TODO: put downloads from DiagramPanel in download dropdown
  // TODO: add upload button and put the current upload functionality there, add "upload trio" option
  // const downloadTrio = useDownloadTrio();

  const dropdownItems: DropdownItem[] = [
    { label: "as SVG", onClick: downloadSvg() },
    { label: "as Penrose trio", onClick: useDownloadTrio() },
    { label: "as SVG TeX", onClick: downloadSvgTex() },
    { label: "as PNG", onClick: downloadPng() },
    { label: "as PDF", onClick: downloadPdf() },
  ];

  return (
    <div>
      <DropdownButton items={dropdownItems} label={"download ▼"} />
    </div>
  );
}
