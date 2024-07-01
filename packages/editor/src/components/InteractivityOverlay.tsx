/*
 * A component that lies over the DiagramPanel which displays interactivity handles
 */

import { MutableRefObject, useEffect, useMemo, useRef, useState } from "react";
import { useRecoilValue } from "recoil";
import { RenderState } from "../optimizer/common.js";
import { diagramState } from "../state/atoms.js";
import HoverDisplay from "./HoverDisplay.js";
import InteractiveWidget from "./InteractiveWidget.js";

export interface InteractivityOverlayProps {
  diagramSVG: SVGSVGElement;
  state: RenderState;
  svgTitleCache: Map<string, SVGElement>;
}

export default function InteractivityOverlay(
  props: InteractivityOverlayProps,
): JSX.Element {
  const diagram = useRecoilValue(diagramState);
  const [clickedPath, setClickedPath] = useState<string | null>(null);
  const [hoveredPath, setHoveredPath] = useState<string | null>(null);
  const [pinnedPaths, setPinnedPaths] = useState<Set<string>>(new Set());
  const activeOverlay = useRef<HTMLDivElement | null>(null);

  const clickedElem = useMemo(
    () => (clickedPath ? props.svgTitleCache.get(clickedPath) ?? null : null),
    [props.svgTitleCache, clickedPath],
  );

  const hoveredElem = useMemo(
    () => (hoveredPath ? props.svgTitleCache.get(hoveredPath) ?? null : null),
    [props.svgTitleCache, hoveredPath],
  );

  useEffect(() => {
    const clickables = new Set<Element>();

    const interactables = function* () {
      const translatables =
        props.state.interactivityInfo.translatableShapePaths.keys();
      const scalables = props.state.interactivityInfo.scalableShapePaths.keys();
      for (const path of translatables)
        yield { path, elem: props.svgTitleCache.get(path) };
      for (const path of scalables)
        yield { path, elem: props.svgTitleCache.get(path) };
    };

    for (const { path, elem } of interactables()) {
      if (elem === undefined) {
        continue;
      }

      const elemFamily = Array.from(elem.querySelectorAll("*")) as SVGElement[];
      elemFamily.push(elem);

      for (const member of elemFamily) {
        const mousedownListener = (e: MouseEvent) => {
          setClickedPath(path);
          if (path === hoveredPath) {
            setHoveredPath(null);
          }
        };

        const mouseoverListener = (e: MouseEvent) => {
          setHoveredPath(path === clickedPath ? null : path);
        };

        member.setAttribute("pointer-events", "visiblePainted");
        member.addEventListener("mousedown", mousedownListener);
        member.addEventListener("mouseover", mouseoverListener);

        clickables.add(member);
      }
    }

    const onMousedownBackground = (e: MouseEvent) => {
      if (
        !clickables.has(e.target as Element) &&
        activeOverlay.current &&
        !activeOverlay.current.contains(e.target as Element)
      ) {
        setClickedPath(null);
      }
    };

    const onMouseoverBackground = (e: MouseEvent) => {
      if (!clickables.has(e.target as Element)) {
        setHoveredPath(null);
      }
    };

    document.addEventListener("mousedown", onMousedownBackground);
    document.addEventListener("mouseover", onMouseoverBackground);

    return () => {
      document.removeEventListener("mousedown", onMousedownBackground);
      document.removeEventListener("mouseover", onMouseoverBackground);
    };
  }, [props.svgTitleCache, clickedPath, hoveredPath]);

  useEffect(() => {
    const pinnedPathsArray = diagram.historyLoc
      ? diagram.historyInfo
          ?.get(diagram.historyLoc.sequenceId)
          ?.pinnedInputPaths.keys()
      : undefined;
    if (pinnedPathsArray) {
      setPinnedPaths(new Set(pinnedPathsArray));
    } else {
      setPinnedPaths(new Set());
    }
  }, [diagram.historyInfo]);

  return (
    <div
      style={{
        position: "absolute",
        width: "100%",
        height: "100%",
        pointerEvents: "none",
      }}
      ref={activeOverlay}
    >
      {clickedPath &&
        clickedElem &&
        activeOverlay.current &&
        diagram.historyLoc && (
          <InteractiveWidget
            elem={clickedElem}
            path={clickedPath}
            diagramSVG={props.diagramSVG}
            state={props.state}
            overlay={activeOverlay as MutableRefObject<Element>}
            pinnedPaths={pinnedPaths}
          />
        )}

      {hoveredPath && hoveredElem && activeOverlay.current && (
        <HoverDisplay
          elem={hoveredElem}
          overlay={activeOverlay as MutableRefObject<Element>}
          state={props.state}
          pinned={pinnedPaths.has(hoveredPath)}
        />
      )}
    </div>
  );
}
