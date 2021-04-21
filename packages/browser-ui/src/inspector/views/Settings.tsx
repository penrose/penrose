import * as React from "react";
import IViewProps from "./IViewProps";
const Settings: React.FC<IViewProps> = ({
  settings,
  setSettings,
}: IViewProps) => {
  const onAutostepChange = React.useCallback(
    (e) => {
      setSettings({ ...settings, autoStepSize: parseInt(e.target.value, 10) });
    },
    [setSettings, settings]
  );
  return (
    <div style={{ boxSizing: "border-box", padding: "1em" }}>
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
  );
};
export default Settings;
