import { useEffect } from "react";
import { useRecoilCallback, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import {
  currentAppUser,
  currentWorkspaceState,
  savedFilesState,
} from "../state/atoms.js";
import {
  useDeleteLocalFile,
  useLoadLocalWorkspace,
  useSaveNewWorkspace,
  useSaveWorkspace,
  useSignIn,
} from "../state/callbacks.js";
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
    document.addEventListener("keydown", handleShortcut);

    // Cleanup
    return () => document.removeEventListener("keydown", handleShortcut);
  }, []);
};

export default function SavedFilesBrowser() {
  const savedFiles = useRecoilValue(savedFilesState);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const loadWorkspace = useLoadLocalWorkspace();
  const onDelete = useDeleteLocalFile();
  const saveNewWorkspace = useSaveNewWorkspace();
  const useLogin = useSignIn();
  const currentUser = useRecoilValue(currentAppUser);

  saveShortcutHook();

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
