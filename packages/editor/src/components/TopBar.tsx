import { useRecoilCallback, useRecoilState } from "recoil";
import { WorkspaceLocation, workspaceMetadataSelector } from "../state/atoms";
import { useCompileDiagram } from "../state/callbacks";
import BlueButton from "./BlueButton";

export default function TopBar() {
  const compileDiagram = useCompileDiagram();
  const [workspaceMetadata, setWorkspaceMetadata] = useRecoilState(
    workspaceMetadataSelector
  );
  const saveLocally = useRecoilCallback(({ set }) => () => {
    set(workspaceMetadataSelector, (state) => ({
      ...state,
      location: { kind: "local", saved: true } as WorkspaceLocation,
    }));
  });
  return (
    <nav
      style={{
        display: "flex",
        width: "100%",
        backgroundColor: "#F4F4F4",
        justifyContent: "space-between",
        alignItems: "center",
        padding: "10px",
        boxSizing: "border-box",
      }}
    >
      <div>Penrose</div>
      <div>
        <BlueButton onClick={compileDiagram}>compile</BlueButton>
        {(workspaceMetadata.location.kind === "gist" ||
          !workspaceMetadata.location.saved) && (
          <BlueButton onClick={saveLocally}>save locally</BlueButton>
        )}
      </div>
    </nav>
  );
}
