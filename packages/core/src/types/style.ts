//#region Style AST
// TODO: unify type name convention (e.g. stop using `I` for interfaces and drop some of the Haskell ported types)

import * as ad from "./ad";
import { ASTNode, Identifier, IStringLit } from "./ast";
import { LabelType } from "./substance";

/** Top level type for Style AST */
export type StyProg<T> = ASTNode<T> & {
  tag: "StyProg";
  blocks: HeaderBlock<T>[];
};

// type HeaderBlock = [Header, Block];
export type HeaderBlock<T> = ASTNode<T> & {
  tag: "HeaderBlock";
  header: Header<T>;
  block: Block<T>;
};

export type Block<T> = ASTNode<T> & {
  tag: "Block";
  statements: Stmt<T>[];
};

export type Header<T> = Selector<T> | Namespace<T>;

export type Selector<T> = ASTNode<T> & {
  tag: "Selector";
  head: DeclPatterns<T>;
  with?: DeclPatterns<T>;
  where?: RelationPatterns<T>;
  namespace?: Namespace<T>;
};

// NOTE: Instead of a js array typed child. I explicitly wrap them in an ASTNode so location and ancestry info can be better preserved.
// TODO: consider dropping the suffix pattern. It's a bit confusing, and DeclList would have been clearer.
export type DeclPatterns<T> = ASTNode<T> & {
  tag: "DeclPatterns";
  contents: DeclPattern<T>[];
};

export type Namespace<T> = ASTNode<T> & {
  tag: "Namespace";
  contents: StyVar<T>;
};

export type DeclPattern<T> = ASTNode<T> & {
  tag: "DeclPattern";
  type: StyT<T>;
  id: BindingForm<T>;
};

export type RelationPattern<T> = RelBind<T> | RelPred<T> | RelField<T>;

export type RelField<T> = ASTNode<T> & {
  tag: "RelField";
  name: BindingForm<T>;
  field: Identifier<T>;
  fieldDescriptor?: LabelType;
};

export type RelationPatterns<T> = ASTNode<T> & {
  tag: "RelationPatterns";
  contents: RelationPattern<T>[];
};

export type RelBind<T> = ASTNode<T> & {
  tag: "RelBind";
  id: BindingForm<T>;
  expr: SelExpr<T>;
};

export type RelPred<T> = ASTNode<T> & {
  tag: "RelPred";
  name: Identifier<T>;
  args: PredArg<T>[];
};

export type PredArg<T> = SEBind<T> | RelPred<T>;

// NOTE: the original type is unnecessarily nested and contain type constructor, which is deprecated.
export type StyT<T> = Identifier<T>;
// type StyT = ISTTypeVar | ISTCtor;

export type ISTTypeVar<T> = ASTNode<T> & {
  tag: "STTypeVar";
  contents: STypeVar<T>;
};

export type ISTCtor<T> = ASTNode<T> & {
  tag: "STCtor";
  contents: STypeCtor<T>;
};

export type STypeVar<T> = ISTypeVar<T>;

export type ISTypeVar<T> = ASTNode<T> & {
  tag: "STypeVar";
  typeVarNameS: string;
};

export type STypeCtor<T> = ISTypeCtor<T>;

export type ISTypeCtor<T> = ASTNode<T> & {
  tag: "STypeCtor";
  nameConsS: string;
  argConsS: SArg<T>[];
};

export type SArg<T> = ISAVar<T> | ISAT<T>;

export type ISAVar<T> = ASTNode<T> & {
  tag: "SAVar";
  contents: BindingForm<T>;
};

export type ISAT<T> = ASTNode<T> & {
  tag: "SAT";
  contents: StyT<T>;
};

export type SelExpr<T> =
  | SEBind<T>
  | SEFunc<T>
  | SEValCons<T>
  | SEFuncOrValCons<T>;

export type SEBind<T> = ASTNode<T> & {
  tag: "SEBind";
  contents: BindingForm<T>;
};

export type SEFunc<T> = ASTNode<T> & {
  tag: "SEFunc";
  name: Identifier<T>;
  args: SelExpr<T>[];
};

export type SEValCons<T> = ASTNode<T> & {
  tag: "SEValCons";
  name: Identifier<T>;
  args: SelExpr<T>[];
};
// NOTE: This type is used by the style compiler; since the grammar is ambiguous, the compiler will need to narrow down the type of this node when checking the AST.
export type SEFuncOrValCons<T> = ASTNode<T> & {
  tag: "SEFuncOrValCons";
  name: Identifier<T>;
  args: SelExpr<T>[];
};

export type Stmt<T> = PathAssign<T> | IOverride<T> | Delete<T> | IAnonAssign<T>;

export type PathAssign<T> = ASTNode<T> & {
  tag: "PathAssign";
  type: StyType<T>;
  path: Path<T>;
  value: Expr<T>;
};

export type IOverride<T> = ASTNode<T> & {
  tag: "Override";
  path: Path<T>;
  value: Expr<T>;
};

export type Delete<T> = ASTNode<T> & {
  tag: "Delete";
  contents: Path<T>;
};

export type IAnonAssign<T> = ASTNode<T> & {
  tag: "AnonAssign";
  contents: Expr<T>;
};

export type StyType<T> = ITypeOf<T> | IListOf<T>;

export type ITypeOf<T> = ASTNode<T> & {
  tag: "TypeOf";
  contents: string;
};

export type IListOf<T> = ASTNode<T> & {
  tag: "ListOf";
  contents: string;
};

export type BindingForm<T> = SubVar<T> | StyVar<T>;

export type SubVar<T> = ASTNode<T> & {
  tag: "SubVar";
  contents: Identifier<T>;
};

export type StyVar<T> = ASTNode<T> & {
  tag: "StyVar";
  contents: Identifier<T>;
};

export type Expr<T> =
  // | IIntLit<T>
  | AnnoFloat<T>
  | IStringLit<T>
  | IBoolLit<T>
  | Path<T> // NOTE: changed from EPath
  | ICompApp<T>
  | IObjFn<T>
  | IConstrFn<T>
  // | IAvoidFn<T> // TODO: unimplemented
  | IBinOp<T>
  | IUOp<T>
  | IList<T>
  | ITuple<T>
  | IVector<T>
  | IMatrix<T>
  | IVectorAccess<T>
  | IMatrixAccess<T>
  | IListAccess<T>
  | GPIDecl<T>
  | ILayering<T>
  | IPluginAccess<T>;
// | IThenOp<T>; // TODO: deprecated transformation exprs

export type IIntLit<T> = ASTNode<T> & {
  tag: "IntLit";
  contents: number;
};

// export type IAFloat<T> = ASTNode<T> & {
//     tag: "AFloat";
//     contents: AnnoFloat<T>;
// };

export type IBoolLit<T> = ASTNode<T> & {
  tag: "BoolLit";
  contents: boolean;
};

export type ICompApp<T> = ASTNode<T> & {
  tag: "CompApp";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type IObjFn<T> = ASTNode<T> & {
  tag: "ObjFn";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type IConstrFn<T> = ASTNode<T> & {
  tag: "ConstrFn";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type IAvoidFn<T> = ASTNode<T> & {
  tag: "AvoidFn";
  contents: [string, Expr<T>[]];
};

export type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

// NOTE: unary + operator not parsed, as they don't change values
export type UnaryOp = "UMinus";
export type IBinOp<T> = ASTNode<T> & {
  tag: "BinOp";
  op: BinaryOp;
  left: Expr<T>;
  right: Expr<T>;
};
export type IUOp<T> = ASTNode<T> & {
  tag: "UOp";
  op: UnaryOp;
  arg: Expr<T>;
};

export type IList<T> = ASTNode<T> & {
  tag: "List";
  contents: Expr<T>[];
};

export type ITuple<T> = ASTNode<T> & {
  tag: "Tuple";
  contents: [Expr<T>, Expr<T>];
};

export type IVector<T> = ASTNode<T> & {
  tag: "Vector";
  contents: Expr<T>[];
};

export type IMatrix<T> = ASTNode<T> & {
  tag: "Matrix";
  contents: Expr<T>[];
};

export type IVectorAccess<T> = ASTNode<T> & {
  tag: "VectorAccess";
  contents: [Path<T>, Expr<T>];
};

export type IMatrixAccess<T> = ASTNode<T> & {
  tag: "MatrixAccess";
  contents: [Path<T>, Expr<T>[]];
};

export type IListAccess<T> = ASTNode<T> & {
  tag: "ListAccess";
  contents: [Path<T>, number];
};

export type GPIDecl<T> = ASTNode<T> & {
  tag: "GPIDecl";
  shapeName: Identifier<T>;
  properties: PropertyDecl<T>[];
};

export type ILayering<T> = ASTNode<T> & {
  tag: "Layering";
  below: Path<T>;
  above: Path<T>;
};

export type IPluginAccess<T> = ASTNode<T> & {
  tag: "PluginAccess";
  contents: [string, Expr<T>, Expr<T>];
};

export type IThenOp<T> = ASTNode<T> & {
  tag: "ThenOp";
  contents: [Expr<T>, Expr<T>];
};

export type AnnoFloat<T> = IFix<T> | IVary<T> | IVaryInit<T> | IVaryAD<T>;

export type IFix<T> = ASTNode<T> & {
  tag: "Fix";
  contents: number;
};

export type IVary<T> = ASTNode<T> & {
  tag: "Vary";
};

// Varying float that is initialized at some number as specified by the style-writer
export type IVaryInit<T> = ASTNode<T> & {
  tag: "VaryInit";
  contents: number;
};

export type IVaryAD<T> = ASTNode<T> & {
  tag: "VaryAD";
  contents: ad.Num;
};

export type PropertyDecl<T> = ASTNode<T> & {
  tag: "PropertyDecl";
  name: Identifier<T>;
  value: Expr<T>;
};

// TODO: check how the evaluator/compiler should interact with ASTNode
export type Path<T> =
  | IFieldPath<T>
  | IPropertyPath<T>
  | IAccessPath<T>
  | LocalVar<T>
  | IInternalLocalVar<T>;
// LocalVar is only used internally by the compiler
// Unused
// | ITypePropertyPath<T>;

export type IFieldPath<T> = ASTNode<T> & {
  tag: "FieldPath";
  name: BindingForm<T>;
  field: Identifier<T>;
};

export type IPropertyPath<T> = ASTNode<T> & {
  tag: "PropertyPath";
  name: BindingForm<T>;
  field: Identifier<T>;
  property: Identifier<T>;
};

export type IAccessPath<T> = ASTNode<T> & {
  tag: "AccessPath";
  path: Path<T>;
  indices: Expr<T>[];
};

// COMBAK: This is named inconsistently since the parser calls it `LocalVar`, should be ILocalVar
export type LocalVar<T> = ASTNode<T> & {
  tag: "LocalVar";
  contents: Identifier<T>;
};

export type IInternalLocalVar<T> = ASTNode<T> & {
  // Note, better to not extend ASTNode as it's only used internally by compiler, but breaks parser otherwise
  tag: "InternalLocalVar";
  contents: string;
};
//#endregion
