interface FileInfo {
  contents: string;
  fileName: string;
}
export interface FileSocketResult {
  substance: FileInfo;
  style: FileInfo;
  domain: FileInfo;
}

export class FileSocket {
  ws: WebSocket;
  onFiles: (files: FileSocketResult) => void;
  constructor(
    addr: string,
    onFiles: (files: FileSocketResult) => void,
    onClose: () => void
  ) {
    this.onFiles = onFiles;
    this.ws = new WebSocket(addr);
    this.ws.onopen = () => console.info("socket opened");
    this.ws.addEventListener("message", this.onMessage);
    this.ws.onclose = () => {
      onClose();
    };
    this.ws.onerror = (e) => {
      console.error("socket error", e);
    };
  }

  onMessage = (e: MessageEvent): void => {
    const parsed = JSON.parse(e.data);
    if (parsed.type === "trio") {
      this.onFiles(parsed);
    }
  };
  getFile = async (path: string): Promise<string | undefined> => {
    return new Promise((resolve /*, reject*/) => {
      this.ws.addEventListener("message", (e) => {
        const parsed = JSON.parse(e.data);
        if (parsed.type === "gotFile") {
          resolve(parsed.contents);
        }
      });
      this.ws.send(JSON.stringify({ type: "getFile", path }));
    });
  };
}
