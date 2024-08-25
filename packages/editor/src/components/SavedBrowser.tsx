import { useRecoilValue } from "recoil";
import { v4 as uuid } from "uuid";
import {
  currentAppUser,
  currentWorkspaceState,
  savedFilesState,
} from "../state/atoms.js";
import {
  useDeleteWorkspace,
  useLoadLocalWorkspace,
  useSaveNewWorkspace,
} from "../state/callbacks.js";
import { logInWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";
import FileButton from "./FileButton.js";

export default function SavedFilesBrowser() {
  const savedFiles = useRecoilValue(savedFilesState);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const loadWorkspace = useLoadLocalWorkspace();
  const onDelete = useDeleteWorkspace();
  const saveNewWorkspace = useSaveNewWorkspace();
  const useLogin = logInWrapper();
  const currentUser = useRecoilValue(currentAppUser);

  return (
    <>
      {currentUser != null ? (
        <div>
          {Object.values(savedFiles)
            // Display most recently modified at the top
            .sort(
              (file1, file2) =>
                file2.metadata.lastModified - file1.metadata.lastModified,
            )
            .map((file) => (
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
                onClick={() =>
                  saveNewWorkspace(
                    currentWorkspace.metadata.id,
                    currentWorkspace,
                  )
                }
              >
                save current workspace
              </BlueButton>
            )}
            {currentWorkspace.metadata.location.kind === "stored" &&
              currentWorkspace.metadata.location.saved && (
                <BlueButton
                  onClick={() => saveNewWorkspace(uuid(), currentWorkspace)}
                >
                  duplicate workspace
                </BlueButton>
              )}
          </div>
        </div>
      ) : (
        <div style={{ margin: "0.5em" }}>
          <p>Please sign in to use saved diagrams!</p>
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        </div>
      )}
    </>
  );
}
