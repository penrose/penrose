import * as React from "react";
import {toHex} from "../../../utils/Util"
import * as _ from "lodash";

interface IProps {
    inputProps: IInputProps,
    eAttr: string;
    eValue: Value<any>; // is this the best typing?
    modAttr(attrname: string, attrval: Value<any>): void;
}

interface IInputProps {
    inputType: string,
    showValue?: string,
    min? : string,
    max? : string,
    minX? : string,
    minY? : string,
    maxX? : string,
    maxY? : string,
    options? : string[]
}

// if we move to not hardcoding canvas size this needs to be changed
// returns number corresponding to x/y coord boundary of canvas if necessary
const toCanvas = (jsonVal: string) : string => {
    // const cdiv = document.getElementsByClassName("Pane1")[0];
    // todo check safety
    // const cwidth = parseInt(window.getComputedStyle(cdiv).getPropertyValue("width"), 10),
    //       cheight = parseInt(window.getComputedStyle(cdiv).getPropertyValue("height"), 10);
    const cwidth = 800,
        cheight = 700;
    switch (jsonVal) {
        case "CANVASL":
            return (-(cwidth) / 2).toString();
        case "CANVASR":
                return (cwidth / 2).toString();
        case "CANVASB":
            return (-(cheight)/ 2).toString();
        case "CANVAST":
            return (cheight/ 2).toString();
        default:
            return jsonVal;
    }
}

class LabeledInput extends React.Component<IProps> {
    public readonly state = {
        eValue: this.props.eValue
    }
    public componentDidUpdate(prevProps: IProps) {
        if (this.props !== prevProps) {
            this.setState({
                eValue: this.props.eValue
            });
        };
    } // todo - will subpath<x> always have typeof x = number?
    public updateAttr = (id: string, evalue: string | number | Color<number> | boolean | SubPath<number>[]) => {
        const newstate = {
            eValue: {
                ...this.state.eValue,
                contents: evalue
            }
        }
        this.setState(newstate);  // will update span values - could be phased out if spans are set manually
        this.props.modAttr(id, newstate.eValue as Value<any>);
    }
    public handleChange = (eattr: string, event: React.ChangeEvent<HTMLInputElement|HTMLSelectElement>) => {
        const teval = (isNaN(Number(event.target.value))) ? event.target.value : +event.target.value; // convert to number if necessary
        this.updateAttr(eattr, teval); // a little hacky - id is name of property
    }
    // when an input corresponding to a part of an attribute is modified instead of an entire attribute
    public handleSubChange = (eAttr: string, pathIndex: number, subpathIndex: number, pointIndex: number, event: React.ChangeEvent<HTMLInputElement>) => {
        // const ptinfo = event.target.id.split("_");
        // const xory = (ptinfo[2] === "x" ? 0 : 1);
        const teval = (isNaN(Number(event.target.value))) ? event.target.value : +event.target.value;
        const path = _.cloneDeep(this.props.eValue.contents);
        const subpath = path[pathIndex];   // get specific subpath
        const pt = subpath.contents[subpathIndex];    // get specific point
        pt.contents[pointIndex] = teval;
        this.updateAttr(eAttr, path);
    }
    public handleCheck = (eattr: string, event: React.ChangeEvent<HTMLInputElement>) => {
        this.updateAttr(eattr, event.target.checked);
    }
    public keyDown = (eattr: string, event: React.KeyboardEvent<HTMLInputElement>) => {
        if (event.keyCode === 13) {
            const etgt = (event.target) as HTMLInputElement;
            const teval = (isNaN(Number(etgt.value))) ? etgt.value : +etgt.value; // convert to number if necessary
            this.updateAttr(eattr, teval);
        }
    }
    public handleColor = (eattr: string, event: React.ChangeEvent<HTMLInputElement>) => {
        const hex = event.target.value;
        this.updateAttr(eattr, {
            tag: "RGBA",
            contents: this.toRGBA(hex)
        })
    }
    public handleOpacity = (eattr: string, event: React.ChangeEvent<HTMLInputElement>) => {
        const colorobj = [...this.props.eValue.contents.contents] as [number, number, number, number];   // make copy
        colorobj[3] = +event.target.value / 100.0;
        this.updateAttr(eattr, {
            tag: "RGBA",
            contents: colorobj
        })
    }
    // https://stackoverflow.com/questions/21646738/convert-hex-to-rgba
    public toRGBA = (hex: string) => {
        const r = parseInt(hex.slice(1, 3), 16) / 255,
            g = parseInt(hex.slice(3, 5), 16) / 255,
            b = parseInt(hex.slice(5, 7), 16) / 255;
        return ([r, g, b, this.props.eValue.contents.contents[3]] as [number, number, number, number])
    }
    // todo - maybe make toCanvas definable in JSON? range doesn't cover all possible use cases for toCanvas
    public makeRange = () => {  
        const {inputProps, eAttr} = this.props;
        return (<input id={eAttr} type= "range" onChange={(e) => this.handleChange(eAttr, e)} min= {toCanvas(inputProps.min!)} max={toCanvas(inputProps.max!)
        } value={_.round(this.props.eValue.contents)} />)
    }
    public makeSubLabel = (id: string, spanval: string, ltxt: string, showValue: boolean) => {
        if (showValue) {
            return (<label htmlFor={id}>
                <span>{spanval}</span>
                {ltxt}
            </label>)
        }
        else return (<label htmlFor={id}>{ltxt}</label>)
    }
    public makePointRange = () => {
            const {inputProps, eAttr} = this.props;
            const subpaths = this.props.eValue.contents;
            // todo - refactor the whole file so you can call makerange() and makelabel() with params
            return (
            <React.Fragment>
                {subpaths.map((subpath: SubPath<number>, index: number) => {
                    const ptarray = subpath.contents;
                    // note - prob will crash on bezier stuff
                    return (
                    <React.Fragment key={"S" + index}>
                    {ptarray.map((pt: Elem<number>, subindex: number)=> {
                        // todo clean up following lines
                        if (pt.tag !== "Pt") throw new Error('No current support for Bezier curves!');
                        const xid = ["S", index.toString(), "x", subindex.toString(), eAttr].join("_");
                        const xltxt = "S" + index.toString() + "x" + subindex.toString();
                        const xspan = this.state.eValue.contents[index].contents[subindex].contents[0].toString();
                        const yspan = this.state.eValue.contents[index].contents[subindex].contents[1].toString();
                        const yid = ["S", index.toString(), "y", subindex.toString(), eAttr].join("_");
                        const yltxt = "S" + index.toString() + "y" + subindex.toString();
                        return (
                            <React.Fragment key={"S" + index + "pt" + subindex}>
                            <div style={{display: "inline-block"}} className="sublabinput">
                                <input id={xid} type="range" onChange={(e) => this.handleSubChange(eAttr, index, subindex, 0, e)} min={toCanvas(inputProps.minX!)}
                                max={toCanvas(inputProps.maxX!)} value={_.round(pt.contents[0] as number)}/>
                                {this.makeSubLabel(xid, xspan, xltxt, (inputProps.showValue === "true"))}
                            </div>
                            <div className="sublabinput" style={{display: "inline-block"}} >
                                <input id={yid} type="range" onChange={(e) => this.handleSubChange(eAttr, index, subindex, 1, e)} min={toCanvas(inputProps.minY!)}
                                max={toCanvas(inputProps.maxY!)} value={_.round(pt.contents[1] as number)}/>
                                {this.makeSubLabel(yid, yspan, yltxt, (inputProps.showValue === "true"))}
                            </div>
                            </React.Fragment>
                        )
                        })} </React.Fragment>)
                })}
            </React.Fragment>)
            
        }
    public makeText = () => {
        const {eAttr} = this.props;
        // set up to only trigger on enter
        // known bug: setting customizable Text/Label string ppty will not work
        // This is because MathJax is only run once, according to Katherine.
        // don't think this can be fixed here.
        return (<input type="text" id={eAttr} defaultValue={this.props.eValue.contents} onKeyDown={(e) => this.keyDown(eAttr, e)}/>)
    }
    // can be used in images
    public makeURL = () => {
        const {eAttr} = this.props;
        // set up to only trigger on enter
        return (<input type="url" id={eAttr} defaultValue={this.props.eValue.contents} onKeyDown={(e) => this.keyDown(eAttr, e)}/>)
    }
    public makeNumber = () => {
        const {inputProps, eAttr} = this.props;
        return (<input type="number" id={eAttr}  onChange={(e) => this.handleChange(eAttr, e)} min={(inputProps.min) ? toCanvas(inputProps.min) : ""}
         max={(inputProps.max) ? toCanvas(inputProps.max) : ""} value={this.props.eValue.contents}/>)
    }
    public makeCheckbox = () => {
        const {eAttr} = this.props;
        return (<input type="checkbox" id={eAttr} checked={this.props.eValue.contents} onChange={(e) => this.handleCheck(eAttr, e)}/>)
    }
    public makeColor = () => {
        // todo check to make sure it won't break if opacity has more than two sig figs 
        const {eAttr, inputProps} = this.props;
        const cid = "c_" + eAttr;
        const oid = "o_" + eAttr;
        return (
            <React.Fragment>
                <div className="sublabinput" style={{display: "inline-block"}}>
                    <input type= "color" id={cid} value={toHex(this.props.eValue.contents)} onChange={(e) => this.handleColor(eAttr, e)} />
                    {this.makeSubLabel(cid, this.getSpan(), eAttr, (inputProps.showValue === "true"))}
                </div>
                <div className="sublabinput" style={{display: "inline-block"}}>
                    <input type= "number" id={oid} style={{height: "21px", marginLeft: "5px"}} min="0" max="100" 
                    value={_.round(100 * this.props.eValue.contents.contents[3])} onChange={(e) => this.handleOpacity(eAttr, e)}/>
                    {this.makeSubLabel(oid, this.getSpan(), "opacity", false)}
                </div>
            </React.Fragment>
        )
    }
    public makeSelect = () => {
        const {inputProps, eAttr} = this.props;
        if (!inputProps.hasOwnProperty("options")) throw new Error("Select input type must have enumerated options.")
        return (<select id={eAttr} onChange={(e) => this.handleChange(eAttr, e)}>
            {inputProps.options!.map((option: string) => (<option key={option} value={option}>{option}</option>))} value={this.props.eValue.contents} </select>)
    }
    // does necessary conversions to display value
    // todo refactor to take params
    public getSpan = () => {
        const {inputType} = this.props.inputProps;
        if (inputType === "color") return toHex(this.state.eValue.contents)
        else if (inputType === "range") return _.round(this.state.eValue.contents)
        else return this.state.eValue.contents.toString();
    }
    public makeSpan = () => {
        // will display color as hex and checkbox as true/false
        return (<span style={{display: "block"}}>{this.getSpan()}</span>)
    }

    public makeLabel = () => {
        if (this.props.inputProps.inputType === "ptrange") return     // don't need to return anything b/c we need to make individual labels
        // nb - this is a quick fix while we have only one input type that has multiple subinputs
        // if this changes we may want to refactor so this doesn't become a wall of if statements.
        else if (this.props.inputProps.inputType === "color") return
        else if (this.props.inputProps.showValue === "true") return (<label htmlFor={this.props.eAttr}>{this.makeSpan()}{this.props.eAttr}</label>)
        else return (<label htmlFor={this.props.eAttr}>{this.props.eAttr}</label>)
    }
    // read desired input type and return appropriate input element
    public makeInput = () => {
        const {inputType} = this.props.inputProps;
        switch (inputType) {
            case ("range") :
                return (this.makeRange())
            case ("ptrange") :
                return (this.makePointRange())
            case ("color") :
                return (this.makeColor())
            case ("select") :
                return (this.makeSelect())
            case ("checkbox") :
                return (this.makeCheckbox())
            case ("text") :
                return (this.makeText())
            case ("number") :
                return (this.makeNumber())
            case ("url") :
                return (this.makeURL())
            default:
                throw new Error("Invalid input type " + inputType + " .");
        }
    }
    public makeInputAndLabel = () => {
        if (this.props.inputProps.inputType === "ptrange") {
            return (
                <React.Fragment>
                    {this.makeInput()}
                    {this.makeLabel()}
                </React.Fragment>
            )
        }
        else {
            return (<div style={{display: "inline-block"}} className="labinput">
            {this.makeInput()}
            {this.makeLabel()}
            </div>)
        }
    }
    public render() {
        return this.makeInputAndLabel();
    }
}
export default LabeledInput;