import { useEffect } from "react";
import { useRecoilState, useRecoilStateLoadable } from "recoil";
import {
  AppUser,
  AuthModalState,
  currentAppUser,
  currentAuthModalState,
  settingsState,
} from "../state/atoms.js";
import { authObject, signOutWrapper } from "../utils/authUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);

  const [authModalState, setAuthModalState] = useRecoilState<AuthModalState>(
    currentAuthModalState,
  );

  // const [loggedIn, setLoggedIn] = useState(null);
  const [appUserState, setAppUserState] =
    useRecoilState<AppUser>(currentAppUser);

  useEffect(() => {
    authObject.onAuthStateChanged((user) => {
      // https://github.com/firebase/firebase-js-sdk/issues/5722
      const userCopy = JSON.parse(JSON.stringify(user));
      setAppUserState(userCopy);
    });
  }, []);

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
      {appUserState != null ? (
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
