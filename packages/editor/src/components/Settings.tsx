import { useRecoilStateLoadable, useRecoilValue } from "recoil";
import { currentAppUser, settingsState } from "../state/atoms.js";
import { logInWrapper, signOutWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const currentUser = useRecoilValue(currentAppUser);
  const useLogin = logInWrapper();
  const useLogout = signOutWrapper();

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
      <div>
        interactive mode (experimental) <br />
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "Off"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "Off",
            }));
          }}
        />
        <label>Off</label>
        <br />
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "EditMode"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "EditMode",
            }));
          }}
        />
        <label>Edit Mode</label>
        <br />
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "PlayMode"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "PlayMode",
            }));
          }}
        />
        <label>Play Mode</label>
      </div>
      {settings.contents.github === null && (
      {currentUser != null ? (
        <div style={{ margin: "10px" }}>
          {" "}
          <BlueButton onClick={useLogout}>Sign Out</BlueButton>
        </div>
      ) : (
        <div style={{ margin: "10px" }}>
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        </div>
      )}
    </div>
  );
}
