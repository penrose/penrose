type Expr = IIntLit | IAFloat | IStringLit | IBoolLit | IEPath | ICompApp | IObjFn | IConstrFn | IAvoidFn | IBinOp | IUOp | IList | ITuple | IListAccess | ICtor | ILayering | IPluginAccess | IThenOp;

interface IIntLit {
  tag: "IntLit";
  contents: number;
}

interface IAFloat {
  tag: "AFloat";
  contents: AnnoFloat;
}

interface IStringLit {
  tag: "StringLit";
  contents: string;
}

interface IBoolLit {
  tag: "BoolLit";
  contents: boolean;
}

interface IEPath {
  tag: "EPath";
  contents: Path;
}

interface ICompApp {
  tag: "CompApp";
  contents: [string, Expr[]];
}

interface IObjFn {
  tag: "ObjFn";
  contents: [string, Expr[]];
}

interface IConstrFn {
  tag: "ConstrFn";
  contents: [string, Expr[]];
}

interface IAvoidFn {
  tag: "AvoidFn";
  contents: [string, Expr[]];
}

interface IBinOp {
  tag: "BinOp";
  contents: [BinaryOp, Expr, Expr];
}

interface IUOp {
  tag: "UOp";
  contents: [UnaryOp, Expr];
}

interface IList {
  tag: "List";
  contents: Expr[];
}

interface ITuple {
  tag: "Tuple";
  contents: [Expr, Expr];
}

interface IListAccess {
  tag: "ListAccess";
  contents: [Path, number];
}

interface ICtor {
  tag: "Ctor";
  contents: [string, PropertyDecl[]];
}

interface ILayering {
  tag: "Layering";
  contents: [Path, Path];
}

interface IPluginAccess {
  tag: "PluginAccess";
  contents: [string, Expr, Expr];
}

interface IThenOp {
  tag: "ThenOp";
  contents: [Expr, Expr];
}

type AnnoFloat = IFix | IVary;

interface IFix {
  tag: "Fix";
  contents: number;
}

interface IVary {
  tag: "Vary";
}

type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

type UnaryOp = "UPlus" | "UMinus";

type PropertyDecl = IPropertyDecl;

type IPropertyDecl = [string, Expr];

type Path = IFieldPath | IPropertyPath;

interface IFieldPath {
  tag: "FieldPath";
  contents: [BindingForm, string];
}

interface IPropertyPath {
  tag: "PropertyPath";
  contents: [BindingForm, string, string];
}

type Var = IVarConst;

type IVarConst = string;

type BindingForm = IBSubVar | IBStyVar;

interface IBSubVar {
  tag: "BSubVar";
  contents: Var;
}

interface IBStyVar {
  tag: "BStyVar";
  contents: StyVar;
}

type StyVar = IStyVar;

type IStyVar = string;
