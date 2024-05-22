import { useEffect, useState } from "react";
import { useRecoilState, useRecoilStateLoadable } from "recoil";
import {
  AuthModalState,
  currentAuthModalState,
  settingsState,
} from "../state/atoms.js";
import { authObject, signOutWrapper } from "../utils/authUtils.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  // const signOut = useCallback(() => {
  //   setSettings((settings) => ({ ...settings, github: null }));
  // }, []);

  const [authModalState, setAuthModalState] = useRecoilState<AuthModalState>(
    currentAuthModalState,
  );

  // idk if this actually works, would like to test more
  // just need to fix the type err
  const [loggedIn, setLoggedIn] = useState(null);

  useEffect(() => {
    const unsubscribe = authObject.onAuthStateChanged((user) => {
      setLoggedIn(user);
    });

    return unsubscribe; // Clean up the observer on component unmount
  }, []);

  // useEffect(() => {
  //   const currentUser = authObject.currentUser;
  //   if (currentUser != null) {
  //     setLoggedIn(true);
  //   }
  // }, []);

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
      {loggedIn != null ? (
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
