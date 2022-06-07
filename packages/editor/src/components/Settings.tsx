import { useCallback } from "react";
import { useRecoilStateLoadable } from "recoil";
import { settingsState } from "../state/atoms";
import { useSignIn } from "../state/callbacks";
import BlueButton from "./BlueButton";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const signIn = useSignIn();
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
          <BlueButton onClick={signIn}>sign into GitHub</BlueButton>
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
