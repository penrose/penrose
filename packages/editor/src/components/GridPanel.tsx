import { ThemeProvider } from "@material-ui/core";
import { Grid, penroseBlue } from "@penrose/components";
import { range } from "lodash";
import { useEffect, useRef, useState } from "react";
import { useRecoilValue } from "recoil";
import {
  currentRogerState,
  diagramMetadataSelector,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms";
import { generateVariation } from "../state/variation";
import { pathResolver } from "./DiagramPanel";

export default function GridPanel() {
  const { variation, source } = useRecoilValue(diagramMetadataSelector);
  const { substance, style, domain } = source;
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);
  const { gridSize } = useRecoilValue(settingsState);
  const [variations, setVariations] = useState<string[]>([]);
  const prevGridSize = useRef(gridSize);
  useEffect(() => {
    prevGridSize.current = gridSize;
  }, [gridSize]);

  useEffect(() => {
    if (gridSize > prevGridSize.current)
      setVariations(
        range(gridSize).map((i) => (i === 0 ? variation : generateVariation()))
      );
  }, [gridSize]);
  useEffect(() => {
    setVariations(
      range(gridSize).map((i) => (i === 0 ? variation : generateVariation()))
    );
  }, [variation]);
  return (
    <div>
      <ThemeProvider theme={penroseBlue}>
        <Grid
          onStateUpdate={() => {}}
          imageResolver={(path) =>
            pathResolver(path, rogerState, workspaceMetadata)
          }
          metadata={(i: number) => [
            {
              name: "Variation",
              data: variations[i],
            },
          ]}
          diagrams={range(gridSize).map((i) => ({
            substance,
            style,
            domain,
            variation: variations[i],
          }))}
        />
      </ThemeProvider>
    </div>
  );
}
