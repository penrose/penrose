import { useRecoilValue } from "recoil";
import { settingsState, workspaceMetadataSelector } from "../state/atoms.js";
import { useDownloadTrio } from "../state/callbacks.js";
import { DropdownButton, DropdownItem } from "./DropdownButton.js";

export default function DownloadButton() {
  // TODO: put downloads from DiagramPanel in download dropdown
  // TODO: add upload button and put the current upload functionality there, add "upload trio" option
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const settings = useRecoilValue(settingsState);
  const downloadTrio = useDownloadTrio();

  const dropdownItems: DropdownItem[] = [
    {
      label: "download SVG",
      onClick: () => {}, // TODO
    },
    {
      label: "download trio",
      onClick: downloadTrio,
    },
  ];

  return (
    <div>
      <DropdownButton items={dropdownItems} label={"download ▼"} />
    </div>
  );
}
