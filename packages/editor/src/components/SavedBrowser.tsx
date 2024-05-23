import { sendEmailVerification } from "firebase/auth";
import toast from "react-hot-toast";
import { useRecoilValue } from "recoil";
import {
  currentAppUser,
  localFilesState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import {
  useDeleteLocalFile,
  useDuplicate,
  useLoadLocalWorkspace,
  useSaveLocally,
} from "../state/callbacks.js";
import { authObject } from "../utils/firebaseUtils.js";
import { OpenModalButton } from "./AuthWindows.js";
import BlueButton from "./BlueButton.js";
import FileButton from "./FileButton.js";

export default function SavedFilesBrowser() {
  const localFiles = useRecoilValue(localFilesState);
  const currentWorkspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const loadWorkspace = useLoadLocalWorkspace();
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const saveLocally = useSaveLocally();
  const duplicate = useDuplicate();
  const onDelete = useDeleteLocalFile();
  const currentUser = useRecoilValue(currentAppUser);

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
          {Object.values(localFiles).map((file) => (
            <FileButton
              key={file.id}
              onClick={() => loadWorkspace(file.id)}
              isFocused={file.id === currentWorkspaceMetadata.id}
              onDelete={() => onDelete(file)}
            >
              {file.name}
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
