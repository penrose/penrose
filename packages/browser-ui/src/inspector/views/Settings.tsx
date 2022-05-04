import * as React from "react";
import IViewProps from "./IViewProps";
const Settings: React.FC<IViewProps> = ({
  settings,
  setSettings,
  reset,
}: IViewProps) => {
  const onAutostepChange = React.useCallback(
    (e) => {
      setSettings({ ...settings, autoStepSize: parseInt(e.target.value, 10) });
    },
    [setSettings, settings]
  );
  const onVariationChange = React.useCallback(
    (e) => {
      setSettings({ ...settings, variation: e.target.value });
    },
    [setSettings, settings]
  );
  return (
    <div style={{ boxSizing: "border-box", padding: "1em" }}>
      <div>
        <label>
          autostep increment size{" "}
          <input
            type="number"
            placeholder={"default 10000"}
            value={settings.autoStepSize}
            onChange={onAutostepChange}
          />
        </label>
      </div>
      <div>
        <label>
          variation{" "}
          <input
            type="string"
            value={settings.variation}
            onChange={onVariationChange}
            onKeyPress={(e) => {
              if (e.key === "Enter") {
                reset();
              }
            }}
          />
        </label>
      </div>
    </div>
  );
};
export default Settings;
