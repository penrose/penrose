import { useRecoilValue } from "recoil";
import { localFilesState, workspaceMetadataSelector } from "../state/atoms";
import { useLoadLocalWorkspace } from "../state/callbacks";

export default function LocalFilesBrowser() {
  const localFiles = useRecoilValue(localFilesState);
  const currentWorkspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const loadWorkspace = useLoadLocalWorkspace();
  return (
    <div>
      {Object.values(localFiles).map((file) => (
        <div key={file.id} onClick={() => loadWorkspace(file.id)}>
          {file.id === currentWorkspaceMetadata.id && "*"} {file.name}
        </div>
      ))}
    </div>
  );
}
