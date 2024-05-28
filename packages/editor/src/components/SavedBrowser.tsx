import { useEffect } from "react";
import { useRecoilCallback, useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import { currentWorkspaceState, savedFilesState } from "../state/atoms.js";
import {
  useDeleteLocalFile,
  useLoadLocalWorkspace,
  useSaveNewWorkspace,
  useSaveWorkspace,
} from "../state/callbacks.js";
import { authObject, resendVerificationEmail } from "../utils/firebaseUtils.js";
import { OpenModalButton } from "./AuthWindows.js";
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

  saveShortcutHook();

  return (
    <>
      {authObject.currentUser != null &&
      authObject.currentUser.emailVerified ? (
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
          {authObject.currentUser != null &&
          !authObject.currentUser.emailVerified ? (
            <div>
              <h3>Please verify your email!</h3>
              <BlueButton onClick={resendVerificationEmail}>
                Resend Verification
              </BlueButton>
            </div>
          ) : (
            <div>
              <h3>Please sign in to use saved diagrams!</h3>
              <OpenModalButton />
            </div>
          )}
        </div>
      )}
    </>
  );
}
