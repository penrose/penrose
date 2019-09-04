import * as Debug from "debug";

const BASE = "renderer";

class Log {
  private readonly debugInfo = Debug(`${BASE}:info`);
  private readonly debugWarn = Debug(`${BASE}:warn`);
  private readonly debugError = Debug(`${BASE}:error`);
  public info(message: string, source?: string) {
    return this.generateMessage(this.debugInfo, message, source);
  }
  public warn(message: string, source?: string) {
    return this.generateMessage(this.debugWarn, message, source);
  }
  public error(message: string, source?: string) {
    return this.generateMessage(this.debugError, message, source);
  }
  private generateMessage(
    dbgr: Debug.IDebugger,
    message: string,
    source?: string
  ) {
    if (source) {
      return dbgr(source, message);
    } else {
      return dbgr(message);
    }
  }
}

export default new Log();
