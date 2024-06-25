/*
 * A component that lies over the DiagramPanel which displays interactivity handles
 */

import { MutableRefObject, useEffect, useMemo, useRef, useState } from "react";
import { useRecoilValue } from "recoil";
import { RenderState } from "../optimizer/common.js";
import { diagramState } from "../state/atoms";
import HoverDisplay from "./HoverDisplay";
import InteractiveWidget from "./InteractiveWidget";

export interface InteractivityOverlayProps {
  diagramSVG: SVGSVGElement;
  state: RenderState;
}

const getTitleElements = (svg: SVGSVGElement) => {
  return Array.from(svg.querySelectorAll("title"));
};

export default function InteractivityOverlay(
  props: InteractivityOverlayProps,
): JSX.Element {
  const diagram = useRecoilValue(diagramState);
  const [clickedPath, setClickedPath] = useState<string | null>(null);
  const [hoveredPath, setHoveredPath] = useState<string | null>(null);
  const [pinnedPaths, setPinnedPaths] = useState<Set<string>>(new Set());
  const activeOverlay = useRef<HTMLDivElement | null>(null);

  const clickedElem = useMemo(() => {
    for (const titleElem of getTitleElements(props.diagramSVG)) {
      if (titleElem.innerHTML === clickedPath) {
        return titleElem.parentElement as unknown as SVGElement;
      }
    }
    return null;
  }, [props.diagramSVG, clickedPath]);

  const hoveredElem = useMemo(() => {
    for (const titleElem of getTitleElements(props.diagramSVG)) {
      if (titleElem.innerHTML === hoveredPath) {
        return titleElem.parentElement as unknown as SVGElement;
      }
    }
    return null;
  }, [props.diagramSVG, hoveredPath]);

  useEffect(() => {
    const clickables = new Set<Element>();

    for (const titleElem of getTitleElements(props.diagramSVG)) {
      const path = titleElem.innerHTML;
      if (
        !props.state.translatableShapePaths.has(path) &&
        !props.state.scalableShapePaths.has(path)
      ) {
        continue;
      }

      const elem = titleElem.parentElement as unknown as SVGElement;
      const elemFamily = Array.from(elem.querySelectorAll("*")) as SVGElement[];
      elemFamily.push(elem);

      for (const member of elemFamily) {
        member.addEventListener("mousedown", (e: MouseEvent) => {
          setClickedPath(path);
          if (path === hoveredPath) {
            setHoveredPath(null);
          }
        });
        member.addEventListener("mouseover", (e: MouseEvent) => {
          setHoveredPath(path === clickedPath ? null : path);
        });

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
  }, [props.diagramSVG, clickedPath, hoveredPath]);

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
