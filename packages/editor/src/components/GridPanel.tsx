import { Grid } from "@penrose/components";
import { range } from "lodash";
import { useRecoilValue } from "recoil";
import { ThemeProvider } from "styled-components";
import {
  currentRogerState,
  diagramGridState,
  diagramMetadataSelector,
  workspaceMetadataSelector,
} from "../state/atoms";
import { pathResolver } from "./DiagramPanel";

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
          onStateUpdate={() => {}}
          imageResolver={(path) =>
            pathResolver(path, rogerState, workspaceMetadata)
          }
          header={(i) => variations[i]}
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
          gridBoxProps={{
            stateful: false,
          }}
        />
      </ThemeProvider>
    </div>
  );
}
