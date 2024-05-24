import { sendEmailVerification } from "firebase/auth";
import toast from "react-hot-toast";
import { useRecoilValue } from "recoil";
import {
  currentAppUser,
  currentWorkspaceState,
  savedFilesState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import {
  useDeleteLocalFile,
  useDuplicate,
  useLoadLocalWorkspace,
  useSaveLocally,
} from "../state/callbacks.js";
import { authObject, saveNewDiagram } from "../utils/firebaseUtils.js";
import { OpenModalButton } from "./AuthWindows.js";
import BlueButton from "./BlueButton.js";
import FileButton from "./FileButton.js";

export default function SavedFilesBrowser() {
  const savedFiles = useRecoilValue(savedFilesState);
  const currentWorkspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const loadWorkspace = useLoadLocalWorkspace();
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const saveLocally = useSaveLocally();
  const duplicate = useDuplicate();
  const onDelete = useDeleteLocalFile();
  const currentUser = useRecoilValue(currentAppUser);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);

  const coolFunction = () => {
    saveNewDiagram(currentUser?.uid, currentWorkspace);
  };

  // We use authObject.currentUser here as currentAppUser is viewed as
  // json and not a firebase object (????)
  const resendVerificationEmail = () => {
    if (authObject.currentUser != null) {
      console.log(currentUser);
      sendEmailVerification(authObject.currentUser).catch((error) => {
        toast.error(error.message);
      });
    } else {
      toast.error("Please re-login!");
    }
  };

  return (
    <>
      {currentUser != null && currentUser.emailVerified ? (
        <div>
          <BlueButton onClick={coolFunction}> cool button!</BlueButton>
          {Object.values(savedFiles).map((file) => (
            <FileButton
              key={file.metadata.id}
              onClick={() => loadWorkspace(file.metadata.id)}
              isFocused={file.metadata.id === currentWorkspaceMetadata.id}
              onDelete={() => onDelete(file.metadata)}
            >
              {file.metadata.name}
            </FileButton>
          ))}
          <div>
            {(workspaceMetadata.location.kind !== "local" ||
              !workspaceMetadata.location.saved) && (
              <BlueButton onClick={saveLocally}>
                save current workspace
              </BlueButton>
            )}
            {workspaceMetadata.location.kind === "local" &&
              workspaceMetadata.location.saved && (
                <BlueButton onClick={duplicate}>duplicate workspace</BlueButton>
              )}
          </div>
        </div>
      ) : (
        <div style={{ margin: "0.5em" }}>
          {currentUser != null && !currentUser.emailVerified ? (
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
