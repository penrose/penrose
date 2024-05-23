import { useRecoilState, useRecoilStateLoadable } from "recoil";
import {
  AppUser,
  AuthModalState,
  currentAuthModalState,
  settingsState,
} from "../state/atoms.js";
import { signOutWrapper } from "../utils/authUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings({ appUserState }: { appUserState: AppUser }) {
  console.log(appUserState);
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);

  const [authModalState, setAuthModalState] = useRecoilState<AuthModalState>(
    currentAuthModalState,
  );

  const toggleLoginModal = () => {
    setAuthModalState({
      loginIsOpen: !authModalState.loginIsOpen,
      registerIsOpen: authModalState.registerIsOpen,
    });
  };

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
      {appUserState ? (
        <div style={{ margin: "10px" }}>
          {" "}
          <BlueButton onClick={signOutWrapper}>sign out</BlueButton>
        </div>
      ) : (
        <div style={{ margin: "10px" }}>
          <BlueButton onClick={toggleLoginModal}>sign in</BlueButton>
        </div>
      )}
    </div>
  );
}
