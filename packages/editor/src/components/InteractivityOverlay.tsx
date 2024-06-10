/*
 * A component that lies over the DiagramPanel which displays interactivity handles
 */

import { RenderState } from "../worker/common.js";
import DragWidget from "./DragWidget";
import im from "immutable";
import { useEffect } from "react";

export interface InteractivityOverlayProps {
  enabledWidgetPath: string | null;
  diagramSVG: SVGSVGElement;
  state: RenderState
}

export default function InteractivityOverlay(
  props: InteractivityOverlayProps
): JSX.Element {
  return (
    <div style={{
      position: 'absolute',
      width: '100%',
      height: '100%',
      pointerEvents: 'none',
    }}>
      {props.enabledWidgetPath &&
        (
          <DragWidget
            key={props.enabledWidgetPath}
            path={props.enabledWidgetPath}
            diagramSVG={props.diagramSVG}
            state={props.state}
          />
        )
      }
    </div>
  );
}