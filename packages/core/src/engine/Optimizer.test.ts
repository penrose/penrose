import { answer } from "@penrose/optimizer";

describe("optimizer", () => {
  test("gives the answer", async () => {
    expect((await answer())()).toBe(42);
  });
});
