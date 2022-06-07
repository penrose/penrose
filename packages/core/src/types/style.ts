import { ASTNode, Identifier, StringLit } from "./ast";
import { LabelType } from "./substance";

/** Top level type for Style AST */
export type StyProg<T> = ASTNode<T> & {
  tag: "StyProg";
  blocks: HeaderBlock<T>[];
};

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

export type StyT<T> = Identifier<T>;

export type STTypeVar<T> = ASTNode<T> & {
  tag: "STTypeVar";
  contents: STypeVar<T>;
};

export type STCtor<T> = ASTNode<T> & {
  tag: "STCtor";
  contents: STypeCtor<T>;
};

export type STypeVar<T> = ASTNode<T> & {
  tag: "STypeVar";
  typeVarNameS: string;
};

export type STypeCtor<T> = ASTNode<T> & {
  tag: "STypeCtor";
  nameConsS: string;
  argConsS: SArg<T>[];
};

export type SArg<T> = SAVar<T> | SAT<T>;

export type SAVar<T> = ASTNode<T> & {
  tag: "SAVar";
  contents: BindingForm<T>;
};

export type SAT<T> = ASTNode<T> & {
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

export type Stmt<T> = PathAssign<T> | Override<T> | Delete<T> | AnonAssign<T>;

export type PathAssign<T> = ASTNode<T> & {
  tag: "PathAssign";
  type: StyType<T>;
  path: Path<T>;
  value: Expr<T>;
};

export type Override<T> = ASTNode<T> & {
  tag: "Override";
  path: Path<T>;
  value: Expr<T>;
};

export type Delete<T> = ASTNode<T> & {
  tag: "Delete";
  contents: Path<T>;
};

export type AnonAssign<T> = ASTNode<T> & {
  tag: "AnonAssign";
  contents: Expr<T>;
};

export type StyType<T> = TypeOf<T> | ListOf<T>;

export type TypeOf<T> = ASTNode<T> & {
  tag: "TypeOf";
  contents: string;
};

export type ListOf<T> = ASTNode<T> & {
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
  | AnnoFloat<T>
  | StringLit<T>
  | BoolLit<T>
  | Path<T>
  | CompApp<T>
  | ObjFn<T>
  | ConstrFn<T>
  | BinOp<T>
  | UOp<T>
  | List<T>
  | Tuple<T>
  | Vector<T>
  | GPIDecl<T>
  | Layering<T>;

export type IntLit<T> = ASTNode<T> & {
  tag: "IntLit";
  contents: number;
};

export type BoolLit<T> = ASTNode<T> & {
  tag: "BoolLit";
  contents: boolean;
};

export type CompApp<T> = ASTNode<T> & {
  tag: "CompApp";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type ObjFn<T> = ASTNode<T> & {
  tag: "ObjFn";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type ConstrFn<T> = ASTNode<T> & {
  tag: "ConstrFn";
  name: Identifier<T>;
  args: Expr<T>[];
};

export type AvoidFn<T> = ASTNode<T> & {
  tag: "AvoidFn";
  contents: [string, Expr<T>[]];
};

export type BinaryOp = "BPlus" | "BMinus" | "Multiply" | "Divide" | "Exp";

// NOTE: unary + operator not parsed, as they don't change values
export type UnaryOp = "UMinus";

export type BinOp<T> = ASTNode<T> & {
  tag: "BinOp";
  op: BinaryOp;
  left: Expr<T>;
  right: Expr<T>;
};

export type UOp<T> = ASTNode<T> & {
  tag: "UOp";
  op: UnaryOp;
  arg: Expr<T>;
};

export type List<T> = ASTNode<T> & {
  tag: "List";
  contents: Expr<T>[];
};

export type Tuple<T> = ASTNode<T> & {
  tag: "Tuple";
  contents: [Expr<T>, Expr<T>];
};

export type Vector<T> = ASTNode<T> & {
  tag: "Vector";
  contents: Expr<T>[];
};

export type GPIDecl<T> = ASTNode<T> & {
  tag: "GPIDecl";
  shapeName: Identifier<T>;
  properties: PropertyDecl<T>[];
};

export type Layering<T> = ASTNode<T> & {
  tag: "Layering";
  below: Path<T>;
  above: Path<T>;
};

export type ThenOp<T> = ASTNode<T> & {
  tag: "ThenOp";
  contents: [Expr<T>, Expr<T>];
};

export type AnnoFloat<T> = Fix<T> | Vary<T>;

export type Fix<T> = ASTNode<T> & {
  tag: "Fix";
  contents: number;
};

export type Vary<T> = ASTNode<T> & {
  tag: "Vary";
};

export type PropertyDecl<T> = ASTNode<T> & {
  tag: "PropertyDecl";
  name: Identifier<T>;
  value: Expr<T>;
};

export type Path<T> = ASTNode<T> & {
  tag: "Path";
  name: BindingForm<T>;
  members: Identifier<T>[];
  indices: Expr<T>[];
};
