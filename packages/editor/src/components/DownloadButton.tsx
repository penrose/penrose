import { downloadSvg, useDownloadTrio } from "../state/callbacks.js";
import DropdownButton, { DropdownItem } from "./DropdownButton.js";

export default function DownloadButton() {
  // TODO: add upload button and put the current upload functionality there, add "upload trio" option

  const dropdownItems: DropdownItem[] = [
    { label: "as Penrose SVG", onClick: downloadSvg() },
    { label: "as Penrose trio", onClick: useDownloadTrio() },
  ];

  return (
    <div>
      <DropdownButton items={dropdownItems} label={"save locally ▼"} />
    </div>
  );
}
