import { describe, expect, test } from "vitest";
import { topsort } from "./Util.js";

describe("topsort", () => {
  test("simple DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return ["B"];
        }
        throw Error();
      },
      ["C"],
    );
    expect(sorted).toEqual(["A", "B", "C"]);
  });

  test("Complex DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return ["A"];
          case "D":
            return ["E"];
          case "E":
            return ["B", "C"];
        }
        throw Error();
      },
      ["D", "E"],
    );
    expect(sorted).toEqual(["A", "C", "B", "E", "D"]);
  });

  test("Disconnected DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return [""];
          case "D":
            return ["C"];
        }
        throw Error();
      },
      ["D", "E"],
    );
    expect(sorted).toEqual(["C", "D", "A", "B"]);
  });
});
