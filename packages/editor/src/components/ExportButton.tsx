import {
  useDownloadPdf,
  useDownloadPng,
  useDownloadSvgTex,
  useDownloadTrio,
} from "../state/callbacks.js";
import DropdownButton, { DropdownItem } from "./DropdownButton.js";

export default function ExportButton() {
  const dropdownItems: DropdownItem[] = [
    { label: "as SVG TeX", onClick: useDownloadSvgTex() },
    { label: "as PNG", onClick: useDownloadPng() },
    { label: "as PDF", onClick: useDownloadPdf() },
    { label: "as Penrose trio", onClick: useDownloadTrio() },
  ];

  return (
    <div>
      <DropdownButton items={dropdownItems} label={"export ▼"} />
    </div>
  );
}
