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
  onFiles: (files: FileSocketResult) => void,
  onClose: () => void
) => {
  const ws = new WebSocket(addr);
  ws.onopen = () => console.log("socket opened");
  ws.onmessage = (e) => {
    const parsed = JSON.parse(e.data);
    onFiles(parsed);
  };
  ws.onclose = () => {
    onClose();
  };
  ws.onerror = (e) => {
    console.error("socket error", e);
  };
};
