import { ThemeProvider } from "@material-ui/core";
import { Grid, penroseBlue } from "@penrose/components";
import { range } from "lodash";
import { useEffect, useState } from "react";
import { useRecoilValue } from "recoil";
import {
  currentRogerState,
  currentWorkspaceState,
  diagramMetadataSelector,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms";
import { generateVariation } from "../state/variation";
import { pathResolver } from "./DiagramPanel";

export default function GridPanel() {
  const { variation } = useRecoilValue(diagramMetadataSelector);
  const workspace = useRecoilValue(currentWorkspaceState);
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);
  const { files } = workspace;
  const { gridSize } = useRecoilValue(settingsState);
  const [variations, setVariations] = useState<string[]>([]);
  useEffect(() => {
    console.log("resetting variations");
    setVariations(
      range(gridSize).map((i) => (i === 0 ? variation : generateVariation()))
    );
  }, [variation, gridSize]);

  return (
    <div>
      <ThemeProvider theme={penroseBlue}>
        <Grid
          onStateUpdate={() => {}}
          imageResolver={(path) =>
            pathResolver(path, rogerState, workspaceMetadata)
          }
          diagrams={range(gridSize).map((i) => ({
            substance: files.substance.contents,
            style: files.style.contents,
            domain: files.domain.contents,
            variation: variations[i],
          }))}
        />
      </ThemeProvider>
    </div>
  );
}
