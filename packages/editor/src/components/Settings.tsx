import { useEffect, useState } from "react";
import { useRecoilStateLoadable, useRecoilValue } from "recoil";
import {
  currentAppUser,
  savedFilesState,
  settingsState,
} from "../state/atoms.js";
import { countLegacyDiagrams, useRecoverAll } from "../state/callbacks.js";
import { logInWrapper, signOutWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const [numLegacyDiagrams, setNumLegacyDiagrams] = useState<number>(0);
  const currentUser = useRecoilValue(currentAppUser);
  const useLogin = logInWrapper();
  const useLogout = signOutWrapper();
  const recoverAll = useRecoverAll();
  const savedDiagrams = useRecoilValue(savedFilesState);

  useEffect(() => {
    const set = async () => {
      let num = await countLegacyDiagrams(savedDiagrams);
      setNumLegacyDiagrams(num);
    };
    set();
  }, [savedDiagrams]);

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

      {currentUser != null ? (
        <div style={{ margin: "10px" }}>
          <BlueButton onClick={useLogout}>Sign Out</BlueButton>
          <p>
            We've migrated to cloud storage! You have {numLegacyDiagrams}{" "}
            unrestored locally stored diagrams.
          </p>
          {numLegacyDiagrams > 0 && (
            <BlueButton onClick={recoverAll}>Recover All</BlueButton>
          )}
        </div>
      ) : (
        <div style={{ margin: "10px" }}>
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        </div>
      )}
    </div>
  );
}
