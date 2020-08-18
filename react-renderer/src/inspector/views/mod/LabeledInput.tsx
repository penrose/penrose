import * as React from "react";
import {toHex, toScreen} from "../../../Util";
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
    }
    public updateAttr = (id: string, evalue: string | number | Color | boolean) => {
        // this.setState((prevState) => ({
        //     eValue: {
        //         ...prevState.eValue,
        //         contents: evalue
        //     }
        // }))
        
        // this way is not safe apparently. https://stackoverflow.com/questions/43638938/updating-an-object-with-setstate-in-react
        // however, the alternative is above and gives an error!!! 
        // anyway, this is a possible source of a bug
        const newstate = {
            eValue: {
                ...this.state.eValue,
                contents: evalue
            }
        }
        this.setState(newstate);  // will update span values - could be phased out if spans are set manually
        this.props.modAttr(id, newstate.eValue as Value<any>);
    }
    public handleChange = (event: React.ChangeEvent<HTMLInputElement|HTMLSelectElement>) => {
        const teval = (isNaN(Number(event.target.value))) ? event.target.value : +event.target.value; // convert to number if necessary
        this.updateAttr(event.target.id, teval); // a little hacky - id is name of property
    }
    public handleCheck = (event: React.ChangeEvent<HTMLInputElement>) => {
        this.updateAttr(event.target.id, event.target.checked);
    }
    public keyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
        if (event.keyCode === 13) {
            const etgt = (event.target) as HTMLInputElement;
            const teval = (isNaN(Number(etgt.value))) ? etgt.value : +etgt.value; // convert to number if necessary
            this.updateAttr(etgt.id, teval);
        }
    }
    public handleColor = (event: React.ChangeEvent<HTMLInputElement>) => {
        const hex = event.target.value;
        this.updateAttr(event.target.id, {
            tag: "RGBA",
            contents: this.toRGBA(hex)
        })
    }
    // https://stackoverflow.com/questions/21646738/convert-hex-to-rgba
    public toRGBA = (hex: string) => {
        const r = parseInt(hex.slice(1, 3), 16) / 255,
            g = parseInt(hex.slice(3, 5), 16) / 255,
            b = parseInt(hex.slice(5, 7), 16) / 255;
        return ([r, g, b, 1] as [number, number, number, number])
        
        // this will destroy alpha specified in style.
        // todo - pass over alpha from style specification? either way color picker prevents alpha styling
        // maybe there is a custom color picker somewhere that uses alpha... 
    }
    // todo - maybe make toCanvas definable in JSON? range doesn't cover all possible use cases for toCanvas
    public makeRange = () => {  
        const {inputProps, eAttr} = this.props;
        return (<input id={eAttr} type= "range" onChange={this.handleChange} min= {toCanvas(inputProps.min!)} max={toCanvas(inputProps.max!)
        } value={_.round(this.props.eValue.contents)} />)
    }
    public makeText = () => {
        const {eAttr} = this.props;
        // set up to only trigger on enter
        // known bug: setting customizable Text/Label string ppty will not work
        // This is because MathJax is only run once, according to Katherine.
        // don't think this can be fixed here.
        return (<input type="text" id={eAttr} defaultValue={this.props.eValue.contents} onKeyDown={this.keyDown}/>)
    }
    // can be used in images
    public makeURL = () => {
        const {eAttr} = this.props;
        // set up to only trigger on enter
        return (<input type="url" id={eAttr} defaultValue={this.props.eValue.contents} onKeyDown={this.keyDown}/>)
    }
    public makeNumber = () => {
        const {inputProps, eAttr} = this.props;
        return (<input type="number" id={eAttr}  onChange={this.handleChange} min={(inputProps.min) ? toCanvas(inputProps.min) : ""}
         max={(inputProps.max) ? toCanvas(inputProps.max) : ""} value={this.props.eValue.contents}/>)
    }
    public makeCheckbox = () => {
        const {eAttr} = this.props;
        return (<input type="checkbox" id={eAttr} checked={this.props.eValue.contents} onChange={this.handleCheck}/>)
    }
    public makeColor = () => {
        const {eAttr} = this.props;
        return (<input type= "color" id={eAttr} value={toHex(this.props.eValue.contents)} onChange={this.handleColor} />)
    }
    public makeSelect = () => {
        const {inputProps, eAttr} = this.props;
        if (!inputProps.hasOwnProperty("options")) throw new Error("Select input type must have enumerated options.")
        return (<select id={eAttr} onChange={this.handleChange}>
            {inputProps.options!.map((option: string) => (<option key={option} value={option}>{option}</option>))} value={this.props.eValue.contents} </select>)
    }
    // does necessary conversions to display value
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
        if (this.props.inputProps.showValue === "true") return (<label htmlFor={this.props.eAttr}>{this.makeSpan()}{this.props.eAttr}</label>)
        else return (<label htmlFor={this.props.eAttr}>{this.props.eAttr}</label>)
    }

    public makeInput = () => {
        const {inputType} = this.props.inputProps;
        switch (inputType) {
            case ("range") :
                return (this.makeRange())
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
    public render() {
        return <div style={{display: "inline-block"}} className="labinput">
            {this.makeInput()}
            {this.makeLabel()}
        </div>
    }
}
export default LabeledInput;