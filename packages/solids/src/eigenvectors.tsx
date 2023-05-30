import { variable } from "@penrose/core";
import {
  DragDropProvider,
  DragDropSensors,
  createDraggable,
} from "@thisbeyond/solid-dnd";
import { createMutable } from "solid-js/store";

const [ox, oy] = [330, 270];

const Draggable = (props: any) => {
  const draggable = createDraggable(props.id);
  return <div use:draggable>draggable</div>;
};

const Arrowhead = (args: { id: string; fill: string }) => (
  <marker
    orient="auto"
    markerWidth="8"
    markerHeight="16"
    refX="1.5"
    refY="2"
    {...args}
  >
    <path d="M 0,0 V4 L2,2 Z"></path>
  </marker>
);

const Vector = ({
  id,
  fill,
  val,
}: {
  val: number[];
  id: string;
  fill: string;
}) => (
  <g>
    <Arrowhead id={id} fill={fill}></Arrowhead>
    <g transform={`translate(${val[0]}, ${val[1]})`}>
      <circle r="22.91" fill="#000" fill-opacity={0.15}></circle>
    </g>
    <line
      marker-end={`url(#${id})`}
      stroke-width={6}
      stroke={fill}
      x1={ox}
      y1={oy}
      x2={val[0]}
      y2={val[1]}
    ></line>
  </g>
);

const vec = (x: number, y: number) => [
  createMutable(variable(x)),
  createMutable(variable(y)),
];

export default () => {
  const [w, h] = [960, 300];
  const v1 = vec(390, 246);
  const v2 = vec(360, 222);

  return (
    <DragDropProvider>
      <DragDropSensors>
        <svg width={w} height={h}>
          <Draggable id={"first"}></Draggable>
          <g>
            <Vector
              id={"primary"}
              fill={"#3498db"}
              val={v1.map((n) => n.val)}
            ></Vector>
            <Vector
              id={"secondary"}
              fill={"#2ecc71"}
              val={v2.map((n) => n.val)}
            ></Vector>
          </g>
        </svg>
      </DragDropSensors>
    </DragDropProvider>
  );
};
