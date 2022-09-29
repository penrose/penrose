import { useRecoilValue } from "recoil";
import { localFilesState, workspaceMetadataSelector } from "../state/atoms";
import {
  useDeleteLocalFile,
  useDuplicate,
  useLoadLocalWorkspace,
  useSaveLocally,
} from "../state/callbacks";
import BlueButton from "./BlueButton";
import FileButton from "./FileButton";

export default function SavedFilesBrowser() {
  const localFiles = useRecoilValue(localFilesState);
  const currentWorkspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const loadWorkspace = useLoadLocalWorkspace();
  const workspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const saveLocally = useSaveLocally();
  const duplicate = useDuplicate();
  const onDelete = useDeleteLocalFile();
  return (
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
          <BlueButton onClick={saveLocally}>save current workspace</BlueButton>
        )}
        {workspaceMetadata.location.kind === "local" &&
          workspaceMetadata.location.saved && (
            <BlueButton onClick={duplicate}>duplicate workspace</BlueButton>
          )}
      </div>
    </div>
  );
}
