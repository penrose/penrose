import { useCallback } from "react";
import { useRecoilStateLoadable } from "recoil";
import { settingsState } from "../state/atoms.js";
import BlueButton from "./BlueButton.js";

interface settingsProps {
  toggleLoginModal: () => void;
}

export default function Settings({
  toggleLoginModal = () => {},
}: settingsProps) {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const signOut = useCallback(() => {
    setSettings((settings) => ({ ...settings, github: null }));
  }, []);
  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <div>
      <div>
        <label>
          debug mode{" "}
          <input
            type="checkbox"
            checked={settings.contents.debugMode}
            onChange={(e) =>
              setSettings((state) => ({
                ...state,
                debugMode: e.target.checked,
              }))
            }
          />
        </label>
      </div>
      <div>
        <label>
          vim mode{" "}
          <input
            type="checkbox"
            checked={settings.contents.vimMode}
            onChange={(e) =>
              setSettings((state) => ({ ...state, vimMode: e.target.checked }))
            }
          />
        </label>
      </div>
      {settings.contents.github === null && (
        <div style={{ margin: "10px" }}>
          <BlueButton onClick={toggleLoginModal}>sign in</BlueButton>
        </div>
      )}
      {settings.contents.github !== null && (
        <div style={{ margin: "10px" }}>
          <div>
            signed into GitHub as{" "}
            <span style={{ fontWeight: "bold" }}>
              {settings.contents.github.username}
            </span>
          </div>
          <BlueButton onClick={signOut}>sign out</BlueButton>
        </div>
      )}
    </div>
  );
}
