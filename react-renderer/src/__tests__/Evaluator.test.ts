import React from "react";
import { render, unmountComponentAtNode } from "react-dom";
import { act } from "react-dom/test-utils";
import { decodeState } from "../Evaluator"


it("decoding an existing state and make sure nothing is undefined", () => {
    const state = require('./state.json');
    const decodedState = decodeState(state.contents[0]);
    console.log(decodedState);
    expect(Object.values(decodeState)).not.toContain(undefined)
});