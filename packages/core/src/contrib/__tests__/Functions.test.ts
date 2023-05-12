import { compDict } from "../Functions";

describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(compDict)) {
      expect(name).toEqual(func.name);
    }
  });
});
