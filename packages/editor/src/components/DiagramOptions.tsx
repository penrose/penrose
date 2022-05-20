import { useRecoilState } from "recoil";
import { diagramMetadataSelector } from "../state/atoms";
import { useCompileDiagram, useStepDiagram } from "../state/callbacks";
import BlueButton from "./BlueButton";

export default function DiagramOptions() {
  const [diagramMetadata, setDiagramMetadata] = useRecoilState(
    diagramMetadataSelector
  );
  const compileDiagram = useCompileDiagram();
  const stepDiagram = useStepDiagram();

  return (
    <div>
      <label>
        variation:{" "}
        <input
          type="text"
          value={diagramMetadata.variation}
          onChange={(e) =>
            setDiagramMetadata((metadata) => ({
              ...metadata,
              variation: e.target.value,
            }))
          }
          onBlur={compileDiagram}
        />
      </label>
      <div>
        <BlueButton
          onClick={() =>
            setDiagramMetadata((metadata) => ({
              ...metadata,
              autostep: !metadata.autostep,
            }))
          }
        >
          autostep ({diagramMetadata.autostep ? "on" : "off"})
        </BlueButton>
        {!diagramMetadata.autostep && (
          <div>
            <BlueButton onClick={stepDiagram}>step</BlueButton>
            <label>
              step size:{" "}
              <input
                type="number"
                value={diagramMetadata.stepSize}
                onChange={(e) =>
                  setDiagramMetadata((metadata) => ({
                    ...metadata,
                    stepSize: parseInt(e.target.value, 10),
                  }))
                }
                onBlur={compileDiagram}
              />
            </label>
          </div>
        )}
      </div>
    </div>
  );
}
