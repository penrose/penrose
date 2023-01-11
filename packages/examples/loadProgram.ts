import fs from "fs";
import path from "path";

export default function loadProgram(programPath: string): string {
  return fs
    .readFileSync(path.join(__dirname, "src", programPath), "utf8")
    .toString();
}
