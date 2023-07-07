import { allWarnings } from "@penrose/core";
import Select from "react-select";
import { useRecoilState } from "recoil";
import { diagramGridState, diagramMetadataSelector } from "../state/atoms.js";
import {
  useCompileDiagram,
  useStepDiagram,
  useStepStage,
} from "../state/callbacks.js";
import BlueButton from "./BlueButton.js";

export default function DiagramOptions() {
  const [diagramMetadata, setDiagramMetadata] = useRecoilState(
    diagramMetadataSelector
  );
  const [{ gridSize }, setSettingsState] = useRecoilState(diagramGridState);
  const compileDiagram = useCompileDiagram();
  const stepDiagram = useStepDiagram();
  const stepStage = useStepStage();

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
        <BlueButton onClick={stepStage} disabled={diagramMetadata.autostep}>
          next stage
        </BlueButton>
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
      <div>
        <label>
          exclude warnings:{" "}
          <Select
            options={allWarnings.map((tag) => ({ val: tag }))}
            isMulti
            isSearchable
            getOptionLabel={({ val }) => val}
            getOptionValue={({ val }) => val}
            value={diagramMetadata.excludeWarnings.map((tag) => ({ val: tag }))}
            onChange={(values) => {
              setDiagramMetadata((metadata) => ({
                ...metadata,
                excludeWarnings: values.map((v) => v.val),
              }));
            }}
          />
        </label>
      </div>
      <div>
        <label>
          grid size:{" "}
          <input
            type="range"
            min="1"
            max="30"
            value={gridSize}
            onChange={(e) =>
              setSettingsState((settings) => ({
                ...settings,
                gridSize: parseInt(e.target.value, 10),
              }))
            }
          />
          <output>{gridSize}</output>
        </label>
      </div>
    </div>
  );
}
