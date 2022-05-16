import { useRecoilState } from "recoil";
import {
  localFileSystem,
  useLoadWorkspace,
  useOpenFileInWorkspace,
} from "../state/atoms";
import FileButton from "./FileButton";

export default function FileBrowser({}) {
  const [fileSystem] = useRecoilState(localFileSystem);
  const openFileInWorkspace = useOpenFileInWorkspace();
  const loadWorkspace = useLoadWorkspace();
  return (
    <div>
      <h1>files</h1>
      <div>
        <h2>workspaces</h2>
        <div>
          {Object.values(fileSystem.workspace).map((workspace) => (
            <FileButton
              key={workspace.id}
              name={workspace.name}
              onClick={() => loadWorkspace(workspace)}
            />
          ))}
        </div>
      </div>
    </div>
  );
}
