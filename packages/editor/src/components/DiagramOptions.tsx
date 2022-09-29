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
          onClick={() => {
            setDiagramMetadata((metadata) => ({
              ...metadata,
              autostep: !metadata.autostep,
            }));
          }}
        >
          autostep ({diagramMetadata.autostep ? "on" : "off"})
        </BlueButton>
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
            />
          </label>
        </div>
      </div>
      <div>
        <label>
          interactive mode{" "}
          <input
            type="checkbox"
            checked={diagramMetadata.interactive}
            onChange={(e) =>
              setDiagramMetadata((metadata) => ({
                ...metadata,
                interactive: e.target.checked,
              }))
            }
          />
        </label>
      </div>
    </div>
  );
}
