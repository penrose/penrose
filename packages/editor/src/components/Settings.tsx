import { useRecoilStateLoadable, useRecoilValue } from "recoil";
import { currentAppUser, settingsState } from "../state/atoms.js";
import { useSignIn } from "../state/callbacks.js";
import { signOutWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const currentUser = useRecoilValue(currentAppUser);
  const useLogin = useSignIn();
  const useLogout = signOutWrapper();
  // console.log(authObject.currentUser);

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
