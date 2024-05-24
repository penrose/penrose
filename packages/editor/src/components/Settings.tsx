import { signOut } from "firebase/auth";
import toast from "react-hot-toast";
import {
  useRecoilStateLoadable,
  useRecoilValue,
  useResetRecoilState,
  useSetRecoilState,
} from "recoil";
import {
  currentAppUser,
  currentWorkspaceState,
  defaultWorkspaceState,
  diagramState,
  settingsState,
} from "../state/atoms.js";
import { authObject } from "../utils/firebaseUtils.js";
import { OpenModalButton } from "./AuthWindows.js";
import BlueButton from "./BlueButton.js";

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const currentUser = useRecoilValue(currentAppUser);
  const resetDiagramState = useResetRecoilState(diagramState);
  const setCurrentWorkspaceState = useSetRecoilState(currentWorkspaceState);

  const signOutWrapper = () => {
    signOut(authObject)
      .then(() => {
        toast.success("logged out");
      })
      .catch((error) => {
        toast.error("Error: Could not log out");
      });

    resetDiagramState();
    setCurrentWorkspaceState(defaultWorkspaceState());
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
      {currentUser != null ? (
        <div style={{ margin: "10px" }}>
          {" "}
          <BlueButton onClick={signOutWrapper}>sign out</BlueButton>
        </div>
      ) : (
        <div style={{ margin: "10px" }}>
          <OpenModalButton />
        </div>
      )}
    </div>
  );
}
