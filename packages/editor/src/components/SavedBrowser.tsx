import { debounce } from "lodash";
import { useCallback, useEffect, useRef } from "react";
import { useRecoilCallback, useRecoilState, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import {
  autosaveTimerState,
  currentAppUser,
  currentWorkspaceState,
  savedFilesState,
} from "../state/atoms.js";
import {
  useDeleteWorkspace,
  useLoadLocalWorkspace,
  useSaveNewWorkspace,
  useSaveWorkspace,
} from "../state/callbacks.js";
import { logInWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";
import FileButton from "./FileButton.js";

const saveShortcutHook = () => {
  const saveWorkspace = useSaveWorkspace();

  // Need useRecoilCallback for snapshot, otherwise currentWorkspace outdated
  const handleShortcut = useRecoilCallback(
    ({ snapshot }) =>
      async ({
        repeat,
        metaKey,
        ctrlKey,
        key,
      }: {
        repeat: boolean;
        metaKey: boolean;
        ctrlKey: boolean;
        key: string;
      }) => {
        const currentWorkspace = snapshot.getLoadable(
          currentWorkspaceState,
        ).contents;
        if (repeat) return;
        // Cmd+s or Cntrl+s
        if (
          (metaKey || ctrlKey) &&
          key === "s" &&
          currentWorkspace.metadata.location.kind == "stored" &&
          !currentWorkspace.metadata.location.saved
        ) {
          saveWorkspace();
        }
      },
  );

  useEffect(() => {
    document.addEventListener("keydown", handleShortcut, { passive: false });

    // Cleanup
    return () => document.removeEventListener("keydown", handleShortcut);
  }, []);
};

const autosaveHook = () => {
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const isInitialRender = useRef(true);
  const saveWorkspace = useSaveWorkspace();
  const [autosaveTimerValue, autosaveTimerSetter] =
    useRecoilState(autosaveTimerState);

  // Callback so debounce works
  const autosaveLogic = useCallback(
    debounce(async () => {
      console.log(autosaveTimerValue);
      // Reset autosave timer
      if (autosaveTimerValue != null) {
        clearTimeout(autosaveTimerValue);
        // console.log("cleared");
      }
      // Set new timer, 5 seconds without edit
      const newTimeoutId = setTimeout(() => {
        // console.log("running!");
        // console.log(currentWorkspace.metadata.location.kind);
        // console.log(!currentWorkspace.metadata.location.saved);
        if (
          currentWorkspace.metadata.location.kind == "stored" &&
          !currentWorkspace.metadata.location.saved
        ) {
          saveWorkspace();
          // console.log("autosaving");
        }
      }, 3000);
      // console.log(newTimeoutId);
      autosaveTimerSetter(newTimeoutId);
    }, 500),
    // Otherwise updates to these values won't be reflected in execution
    [autosaveTimerValue, currentWorkspace.metadata],
  );

  useEffect(() => {
    if (isInitialRender.current) {
      isInitialRender.current = false;
      return;
    }
    autosaveLogic();
  }, [
    currentWorkspace.files.substance.contents,
    currentWorkspace.files.style.contents,
    currentWorkspace.files.domain.contents,
  ]);
};

export default function SavedFilesBrowser() {
  const savedFiles = useRecoilValue(savedFilesState);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const loadWorkspace = useLoadLocalWorkspace();
  const onDelete = useDeleteWorkspace();
  const saveNewWorkspace = useSaveNewWorkspace();
  const useLogin = logInWrapper();
  const currentUser = useRecoilValue(currentAppUser);

  saveShortcutHook();
  autosaveHook();

  return (
    <>
      {currentUser != null ? (
        <div>
          {Object.values(savedFiles).map((file) => (
            <FileButton
              key={file.metadata.id}
              onClick={() => loadWorkspace(file.metadata.id)}
              isFocused={file.metadata.id === currentWorkspace.metadata.id}
              onDelete={() => onDelete(file.metadata)}
            >
              {file.metadata.name}
            </FileButton>
          ))}
          <div>
            {(currentWorkspace.metadata.location.kind !== "stored" ||
              !currentWorkspace.metadata.location.saved) && (
              <BlueButton
                onClick={() => saveNewWorkspace(currentWorkspace.metadata.id)}
              >
                save current workspace
              </BlueButton>
            )}
            {currentWorkspace.metadata.location.kind === "stored" &&
              currentWorkspace.metadata.location.saved && (
                <BlueButton onClick={() => saveNewWorkspace(uuid())}>
                  duplicate workspace
                </BlueButton>
              )}
          </div>
        </div>
      ) : (
        <div style={{ margin: "0.5em" }}>
          <h3>Please sign in to use saved diagrams!</h3>
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        </div>
      )}
    </>
  );
}
