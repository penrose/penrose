// @vitest-environment jsdom
import { afterEach, describe, expect, test, vi } from "vitest";
import { makeTranslateOnMouseDown } from "./InteractionUtils.js";

afterEach(() => {
  document.body.style.cursor = "";
  vi.unstubAllGlobals();
});

function dragFixture(
  translate: (path: string, dx: number, dy: number) => Promise<void>,
  finished = vi.fn(),
) {
  vi.stubGlobal(
    "DOMPoint",
    class {
      constructor(
        public x = 0,
        public y = 0,
      ) {}
      matrixTransform() {
        return this;
      }
    },
  );
  vi.stubGlobal(
    "DOMRect",
    class {
      constructor(
        public x = 0,
        public y = 0,
        public width = 0,
        public height = 0,
      ) {}
      get left() {
        return this.x;
      }
      get top() {
        return this.y;
      }
      get right() {
        return this.x + this.width;
      }
      get bottom() {
        return this.y + this.height;
      }
    },
  );
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg"),
    element = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  svg.getScreenCTM = () => ({ inverse: () => ({}) }) as unknown as DOMMatrix;
  Object.defineProperty(svg, "viewBox", {
    value: { baseVal: { width: 200, height: 100 } },
  });
  element.getBoundingClientRect = () => new DOMRect(10, 10, 20, 20);
  const start = makeTranslateOnMouseDown(
    svg,
    element,
    {
      width: 200,
      height: 100,
      size: [200, 100],
      xRange: [-100, 100],
      yRange: [-50, 50],
    },
    "region",
    translate,
    undefined,
    undefined,
    finished,
  );
  return {
    element,
    start: () =>
      start(new MouseEvent("pointerdown", { clientX: 20, clientY: 20 })),
    finished,
  };
}

describe("native pointer drag cancellation", () => {
  test("completed drags remove cancel listeners and never replay ended translations", () => {
    const translate = vi.fn(async () => {}),
      { start, element, finished } = dragFixture(translate);
    document.body.style.cursor = "crosshair";
    start();
    expect(document.body.style.cursor).toBe("grabbing");
    window.dispatchEvent(new MouseEvent("pointerup"));
    expect(finished).toHaveBeenCalledTimes(1);
    expect(translate).toHaveBeenCalledTimes(1);
    window.dispatchEvent(new MouseEvent("pointercancel"));
    expect(finished).toHaveBeenCalledTimes(1);
    expect(translate).toHaveBeenCalledTimes(1);
    expect(document.body.style.cursor).toBe("crosshair");
    expect(element.style.cursor).toBe("grab");
    start();
    window.dispatchEvent(new MouseEvent("pointercancel"));
    window.dispatchEvent(new MouseEvent("pointerup"));
    expect(finished).toHaveBeenCalledTimes(2);
    expect(translate).toHaveBeenCalledTimes(2);
  });
  test("cancellation drops queued moves after an asynchronous translation", async () => {
    let release!: () => void;
    const pending = new Promise<void>((resolve) => {
        release = resolve;
      }),
      translate = vi.fn(() => pending),
      { start, finished } = dragFixture(translate);
    start();
    window.dispatchEvent(
      new MouseEvent("pointermove", { clientX: 25, clientY: 20 }),
    );
    window.dispatchEvent(
      new MouseEvent("pointermove", { clientX: 35, clientY: 20 }),
    );
    window.dispatchEvent(new MouseEvent("pointercancel"));
    const callsAtCancellation = translate.mock.calls.length;
    release();
    await pending;
    await Promise.resolve();
    window.dispatchEvent(
      new MouseEvent("pointermove", { clientX: 45, clientY: 20 }),
    );
    window.dispatchEvent(new MouseEvent("pointercancel"));
    expect(translate).toHaveBeenCalledTimes(callsAtCancellation);
    expect(finished).toHaveBeenCalledTimes(1);
  });
});
