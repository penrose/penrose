interface FileInfo {
  contents: string;
  fileName: string;
}
export interface FileSocketResult {
  substance: FileInfo;
  style: FileInfo;
  domain: FileInfo;
}
export const FileSocket = (
  addr: string,
  onFiles: (files: FileSocketResult) => void
) => {
  const ws = new WebSocket(addr);
  ws.onopen = () => console.log("socket opened");
  ws.onmessage = (e) => {
    const parsed = JSON.parse(e.data);
    onFiles(parsed);
  };
};
