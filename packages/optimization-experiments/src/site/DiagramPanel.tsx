import { stateToSVG } from "@penrose/bloom/dist/core/utils.js";
import {
  PathResolver,
  State as PenroseState,
  compile,
  prettyPrintFn,
  resample,
} from "@penrose/core";
import { OptOutputs } from "@penrose/core/dist/types/ad";
import { generateVariation } from "@penrose/editor/src/state/variation.js";
import { Trio } from "@penrose/examples/dist/index.js";
import { useEffect, useMemo, useRef, useState } from "react";
import { TrioInfo } from "./TrioSelector.js";
import { StagedOptimizer } from "../Optimizers.js";

class MessageChannelLooper {
  private channel: MessageChannel;
  private callback: () => Promise<boolean>;
  private running: boolean = false;

  constructor(callback: () => Promise<boolean>) {
    this.channel = new MessageChannel();
    this.callback = callback;
  }

  start() {
    this.running = true;

    this.channel.port2.onmessage = async () => {
      if (!this.running) return;
      const shouldContinue = await this.callback();
      if (shouldContinue) {
        this.channel.port1.postMessage(null);
      } else {
        this.running = false;
      }
    };

    this.channel.port1.postMessage(null);
  }

  stop() {
    this.running = false;
  }

  setCallback(callback: () => Promise<boolean>) {
    this.callback = callback;
  }
}

const compileTrio = async (
  trio: Trio,
): Promise<{
  state: PenroseState;
  pathResolver: PathResolver;
}> => {
  const { contents: style, resolver: pathResolver } = trio.style[0];
  const result = await compile({
    ...trio,
    style,
  });
  if (result.isErr()) {
    throw result.error;
  }

  const state = result.value;
  return {
    state,
    pathResolver,
  };
};

export const DiagramPanel = ({
  trioInfo,
  optimizer,
}: {
  trioInfo: TrioInfo;
  optimizer: StagedOptimizer;
}) => {
  const [penroseState, setPenroseState] = useState<PenroseState | null>(null);
  const canvasRef = useRef<HTMLDivElement>(null);
  const renderLooper = useRef<MessageChannelLooper>(
    new MessageChannelLooper(async () => false),
  );
  const [optOutputs, setOptOutputs] = useState<OptOutputs | null>(null);

  const renderCallback = useMemo(() => {
    console.log("new render callback");

    if (!penroseState) return async () => false;

    optimizer.init(penroseState);

    return async () => {
      if (!penroseState) return false;

      const optimizerResult = optimizer.step(penroseState);

      if (optimizerResult.tag !== "Failed") {
        setOptOutputs(optimizerResult.outputs);
      }

      if (canvasRef.current) {
        const svg = stateToSVG(penroseState, {
          pathResolver: trioInfo.resolver,
          texLabels: false,
        });

        const svgElement = await svg;
        if (canvasRef.current.firstElementChild) {
          canvasRef.current.replaceChild(
            svgElement,
            canvasRef.current.firstElementChild,
          );
        } else {
          canvasRef.current.appendChild(svgElement);
        }
      }

      switch (optimizerResult.tag) {
        case "Unconverged":
          return true;

        case "Converged":
        case "Failed":
          return false;
      }
    };
  }, [penroseState, optimizer]);

  useEffect(() => {
    (async () => {
      const { state } = await compileTrio(trioInfo.trio);
      console.log("compiled trio", trioInfo.trio);
      setPenroseState(state);
    })();
  }, [trioInfo]);

  useEffect(() => {
    const looper = new MessageChannelLooper(renderCallback);
    looper.start();
    renderLooper.current = looper;
    return () => looper.stop();
  }, [renderCallback]);

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        justifyContent: "center",
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          width: "40%",
          margin: "1em",
        }}
      >
        Variation: {penroseState?.variation}
        {/* resample button */}
        <button
          onClick={() => {
            if (!penroseState) return;
            setPenroseState((s) => {
              if (!s) return null;
              s.variation = generateVariation();
              return resample(s);
            });
            optimizer.init(penroseState);
          }}
        >
          Resample
        </button>
        <div style={{ border: "2px solid black" }} ref={canvasRef} />
      </div>
      {/*  Two columns showing objectives and total, and then penalties and totals */}
      {/* Off to the side */}
      <div style={{ border: "2px solid black", width: "40%" }}>
        <div
          style={{
            display: "grid",
            gridTemplateColumns: "20% 20% 20%",
            justifyContent: "space-evenly",
          }}
        >
          <div>
            <h3>Objectives</h3>
            <hr />
            Total: {optOutputs?.objectives.reduce((a, b) => a + b, 0)}
            <hr />
            {optOutputs?.objectives.map((o, i) => (
              <div key={i}>
                <code>{prettyPrintFn(penroseState?.objFns[i])}</code>: {o}
              </div>
            ))}
          </div>
          <div>
            <h3>Constraints</h3>
            <hr />
            {/* Total penalties */}
            Total:{" "}
            {optOutputs?.constraints.reduce(
              (a, b) => a + Math.max(0, b) * Math.max(0, b),
              0,
            )}
            {/* horizontal separator */}
            <hr />
            {/* List of penalties */}
            {optOutputs?.constraints.map((p, i) => (
              <div key={i} style={{ color: p > 0 ? "red" : "inherit" }}>
                <code>
                  {penroseState?.constrFns[i]
                    ? prettyPrintFn(penroseState?.constrFns[i])
                    : ""}
                </code>
                : {p}
              </div>
            ))}
          </div>
          <div>
            <h3>Phi</h3>
            <hr />
            {/* Total phi */}
            Total: {optOutputs?.phi}
            {/* horizontal separator */}
          </div>
        </div>
      </div>
    </div>
  );
};
