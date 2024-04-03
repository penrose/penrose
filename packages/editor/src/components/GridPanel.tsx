import { Grid } from "@penrose/components";
import { range } from "lodash";
import { useRecoilValue } from "recoil";
import { ThemeProvider } from "styled-components";
import {
  currentRogerState,
  diagramGridState,
  diagramMetadataSelector,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { pathResolver } from "../utils/downloadUtils.js";

export default function GridPanel() {
  const { source } = useRecoilValue(diagramMetadataSelector);
  const { substance, style, domain } = source;
  const { gridSize, variations } = useRecoilValue(diagramGridState);
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);
  return (
    <div>
      <ThemeProvider
        theme={{
          primary: "#C9C9C9",
        }}
      >
        <Grid
          imageResolver={(path: string) =>
            pathResolver(path, rogerState, workspaceMetadata)
          }
          header={(i: number) => variations[i]}
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
