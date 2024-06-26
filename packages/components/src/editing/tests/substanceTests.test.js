import { describe, test } from "vitest";
import { parser } from "../parser/substance/substance";
import { hasNoErrors } from "./testUtils";

describe("Common", () => {
  test("empty", () => {
    let input = "";
    hasNoErrors(parser, input);
  });

  test("comments and white spaces", () => {
    const input = `
        -- Top-level comments
        Set A, B, C, D, E, F, G -- inline comments\r\n
        
        /*
        Subset(B, A)\r
        Subset(C, A)\r\n
        Subset(D, B)
        Subset(E, B)
        Subset(F, C)
        Subset(G, C)
        */
        
        -- Not(Intersecting(E, D))
        Set C
        -- Not(Intersecting(B, C))
        AutoLabel All
        
        /* Other comments */
            `;

    hasNoErrors(parser, input);
  });

  test("fail", () => {
    hasNoErrors(parser, "big huge balls");
  });
});
