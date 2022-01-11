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
  onReceive: (file: string) => void;
  constructor(
    addr: string,
    onFiles: (files: FileSocketResult) => void,
    onClose: () => void
  ) {
    this.onReceive = () => {
      console.log("unused onReceive");
    };
    this.ws = new WebSocket(addr);
    this.ws.onopen = () => console.log("socket opened");
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = () => {
      onClose();
    };
    this.ws.onerror = e => {
      console.error("socket error", e);
    };
    this.onFiles = onFiles;
  }

  onMessage(e: MessageEvent): void {
    const parsed = JSON.parse(e.data);
    if (parsed.type === "trio") {
      this.onFiles(parsed);
    } else if (parsed.type === "gotFile") {
      this.onReceive(parsed.contents);
    } else {
      console.error("unrecognized packet type", parsed);
    }
  }
  async getFile(path: string): Promise<string | null> {
    return new Promise((resolve, reject) => {
      this.onReceive = contents => {
        resolve(contents);
      };
      this.ws.send(JSON.stringify({ type: "getFile", path }));
    });
  }
}
