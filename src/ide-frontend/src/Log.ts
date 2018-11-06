import * as Debug from "debug";

const BASE = "ide";

class Log {
  public info(message: string, source?: string) {
    return this.generateMessage("info", message, source);
  }
  public warn(message: string, source?: string) {
    return this.generateMessage("warn", message, source);
  }
  public error(message: string, source?: string) {
    return this.generateMessage("error", message, source);
  }
  private generateMessage(level: string, message: string, source?: string) {
    const namespace = `${BASE}:${level}`;
    const createDebug = Debug(namespace);
    if (source) {
      return createDebug(source, message);
    } else {
      return createDebug(message);
    }
  }
}

export default new Log();
