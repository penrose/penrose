import { ThemeProvider } from "@material-ui/core";
import { Grid, penroseBlue } from "@penrose/components";
import { range } from "lodash";
import { useRecoilValue } from "recoil";
import {
  currentRogerState,
  currentWorkspaceState,
  diagramMetadataSelector,
  workspaceMetadataSelector,
} from "../state/atoms";
import { generateVariation } from "../state/variation";
import { pathResolver } from "./DiagramPanel";

export default function GridPanel() {
  const { interactive } = useRecoilValue(diagramMetadataSelector);
  const workspace = useRecoilValue(currentWorkspaceState);
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);
  const { files } = workspace;
  return (
    <div>
      <ThemeProvider theme={penroseBlue}>
        <Grid
          onStateUpdate={() => {}}
          imageResolver={(path) =>
            pathResolver(path, rogerState, workspaceMetadata)
          }
          diagrams={range(10).map((i) => ({
            substance: files.substance.contents,
            style: files.style.contents,
            domain: files.domain.contents,
            variation: generateVariation(),
          }))}
        />
      </ThemeProvider>
    </div>
  );
}
