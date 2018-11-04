import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";
declare const MathJax: any;

interface IState {
  w: number;
  h: number;
}

class Label extends React.Component<IGPIPropsDraggable, IState> {
  public readonly state = { w: 0, h: 0 };
  private readonly ref = React.createRef<SVGGElement>();
  public constructor(props: IGPIPropsDraggable) {
    super(props);
    MathJax.Hub.Config({
      skipStartupTypeset: true,
      extensions: ["tex2jax.js", "TeX/AMSmath.js"],
      jax: ["input/TeX", "output/SVG"],
      tex2jax: {
        inlineMath: [["$", "$"], ["\\(", "\\)"]],
        processEscapes: true
      }
    });
  }
  public componentDidMount() {
    this.tex2svg();
  }

  // Sends over the w/h again if it gets overridden during recompilation
  public componentDidUpdate(prevProps: IGPIPropsDraggable) {
    const { shape, onShapeUpdate } = this.props;
    const { w, h } = this.state;
    if (
      onShapeUpdate &&
      ((shape.w === 0 && w !== 0) || (shape.h === 0 && h !== 0))
    ) {
      onShapeUpdate({ ...shape, w, h });
    }
  }
  public tex2svg = () => {
    const wrapper = document.createElement("div");
    wrapper.style.display = "none";
    const cur = this.ref.current;
    const { shape, onShapeUpdate } = this.props;
    const setState = this.setState.bind(this);
    // HACK: Style compiler decides to give empty labels if not specified
    if (cur !== null && this.props.shape.string !== "") {
      wrapper.innerHTML = "$" + this.props.shape.string + "$";
      MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
      MathJax.Hub.Queue(() => {
        const output = wrapper.getElementsByTagName("svg")[0];
        // TODO: need to check whether MathJax returns a non-null response
        cur.innerHTML = output.outerHTML; // need to keep properties in <svg>
        const { width, height } = cur.getBBox();
        if (onShapeUpdate) {
          onShapeUpdate({ ...shape, w: width, h: height });
          setState({ w: width, h: height });
        }
      });
    }
  };

  public render() {
    const props = this.props.shape;
    const { dy, dx, onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([props.x, props.y], canvasSize);
    const { w, h } = props;
    return (
      <g
        transform={`translate(${x - w / 2 - dx},${y - h / 2 + dy})`}
        onMouseDown={onClick}
        ref={this.ref}
      >
        <text>{props.string}</text>
      </g>
    );
  }
}
export default draggable(Label);
