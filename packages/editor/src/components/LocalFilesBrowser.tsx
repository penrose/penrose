import { useRecoilValue } from "recoil";
import styled from "styled-components";
import { localFilesState, workspaceMetadataSelector } from "../state/atoms";
import { useLoadLocalWorkspace } from "../state/callbacks";

const FileButton = styled.button<{ isFocused: boolean }>`
  background-color: ${({ isFocused }) => (isFocused ? "#40b4f7" : "#ffffff")};
  color: ${({ isFocused }) => (isFocused ? "#FFFFFF" : "#4d4d4d")};
  display: block;
  width: 100%;
  border: 0;
  text-align: left;
  font-size: 15px;
  margin: 3px;
  padding: 10px;
  transition: 0.2s;
  :hover {
    transition: 0.2s;
    background-color: ${({ isFocused }) =>
      isFocused ? "#40b4f7" : "rgba(0,0,0,0.1)"};
  }
`;

export default function LocalFilesBrowser() {
  const localFiles = useRecoilValue(localFilesState);
  const currentWorkspaceMetadata = useRecoilValue(workspaceMetadataSelector);
  const loadWorkspace = useLoadLocalWorkspace();
  return (
    <div>
      {Object.values(localFiles).map((file) => (
        <FileButton
          key={file.id}
          onClick={() => loadWorkspace(file.id)}
          isFocused={file.id === currentWorkspaceMetadata.id}
        >
          {file.name}
        </FileButton>
      ))}
    </div>
  );
}
