import * as React from "react";
import { toScreen } from "./Util";
declare const MathJax: any;

interface IState {
  shape: any;
  tempX: number;
  tempY: number;
  origX: number;
  origY: number;
  changed: boolean;
}

class Label extends React.Component<IGPIProps, IState> {
  public static getDerivedStateFromProps(props: IGPIProps, state: IState) {
    if (!state.changed) {
      return { ...state, shape: props.shape };
    }
    return null;
  }
  public readonly state = {
    changed: false,
    shape: {
      x: 0,
      y: 0,
      w: 0,
      h: 0,
      string: "",
      style: "",
      stroke: ""
    },
    tempX: 0,
    tempY: 0,
    origX: 0,
    origY: 0
  };
  private readonly ref = React.createRef<SVGGElement>();
  public constructor(props: IGPIProps) {
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
  public async componentDidMount() {
    this.tex2svg();
  }
  public tex2svg = () => {
    const wrapper = document.createElement("div");
    wrapper.style.display = "none";
    const cur = this.ref.current;
    const t = this;
    if (cur !== null) {
      wrapper.innerHTML = "$" + this.props.shape.string + "$";
      MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
      MathJax.Hub.Queue(() => {
        const output = wrapper.getElementsByTagName("svg")[0];
        cur.innerHTML = output.outerHTML; // need to keep properties in <svg>
        // TODO: send bbox to server
        const { width, height } = cur.getBBox();
        t.setState({ shape: { ...t.state.shape, w: width, h: height } });
      });
    }
  };
  public handleMouseMove = (e: PointerEvent) => {
    const { pageX, pageY } = e;
    const { tempX, tempY } = this.state;
    const dx = tempX - pageX;
    const dy = pageY - tempY;
    const newShape = {
      ...this.state.shape,
      x: this.state.shape.x - dx,
      y: this.state.shape.y - dy
    };
    this.setState({ shape: newShape, tempX: pageX, tempY: pageY });
  };
  public handleMouseDown = (e: React.MouseEvent<any>) => {
    const { x, y } = this.state.shape;
    this.setState({
      tempX: e.pageX,
      tempY: e.pageY,
      origX: x,
      origY: y,
      changed: true
    });
    // These listeners are applied to the document
    // because shape-specific listeners don't fire if there's overlapping issues
    document.addEventListener("mousemove", this.handleMouseMove);
    document.addEventListener("mouseup", this.handleMouseUp);
  };
  public handleMouseUp = () => {
    document.removeEventListener("movemouse", this.handleMouseMove);
    document.removeEventListener("mouseup", this.handleMouseUp);
    const { x, y } = this.state.shape;
    console.log("dx", x - this.state.origX, "dy", this.state.origY - y);
    this.setState({ tempX: 0, tempY: 0, changed: false });
  };
  public render() {
    const props = this.state.shape;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([props.x, props.y], canvasSize);
    const { w, h } = props;
    return (
      <g
        transform={`translate(${x - w / 2},${y - h / 2})`}
        onMouseDown={this.handleMouseDown}
        ref={this.ref}
      >
        <text>{props.string}</text>
      </g>
    );
  }
}
export default Label;
