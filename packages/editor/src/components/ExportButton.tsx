import {
  downloadPdf,
  downloadPng,
  downloadSvgTex,
  useDownloadTrio,
} from "../state/callbacks.js";
import DropdownButton, { DropdownItem } from "./DropdownButton.js";

export default function ExportButton() {
  const dropdownItems: DropdownItem[] = [
    { label: "as SVG TeX", onClick: downloadSvgTex() },
    { label: "as PNG", onClick: downloadPng() },
    { label: "as PDF", onClick: downloadPdf() },
    { label: "as Penrose trio", onClick: useDownloadTrio() },
  ];

  return (
    <div>
      <DropdownButton items={dropdownItems} label={"export ▼"} />
    </div>
  );
}
